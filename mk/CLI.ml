(* Copyright 2016, 2018, 2024, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

module G = Graph
module R = Rules

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of mk, the Plan 9 build system.
 *
 * Main limitations compared to mk:
 *  - no regexp rules
 *    (not worth it, '%' are good enough)
 *  - no special handling for archives
 *    (fast enough nowadays to recreate full archives from scratch)
 *  - no :P:
 *    (it is barely documented anyway, and you can do without)
 *  - no private variables
 *    (I never saw mkfiles using it, and it complicates the parsing of '=')
 *  - no &
 *    (rarely found used, '%' is enough again)
 *  - only one -f is supported, not an array of up to 256 mkfiles
 *    (who uses that? maybe to have mk -f varfile -f mkfile)
 *  - no sequential vs parallel mode, and no parallel for multi targets
 *    (most of the time you give just one target anyway)
 *  - disallow :=<% in more context
 *    (confusing for reader anyway, I prefer to force the user to quote)
 *  - disallow dynamic assignements like X=B ... $X=1
 *    (harder to read, who uses that?)
 *  - disallow dynamic patterns like X=%.o  $X: %.c
 *    (harder to read)
 *  - disallow backquote outside word context (e.g., at the toplevel)
 *    so you can not do `echo <foo.txt`
 *    (harder to read and never used I think)
 *  - no opti like missing intermediate (mk -i)
 *    (I barely understand the algorithm anyway)
 *  - no unicode support
 * 
 * Improvements (IMHO):
 *  - a strict mode where we forbid to redefine variables, use of undefined
 *    variables
 *  - forbid to use list variables in a scalar context (error prone I think)
 *  - a new Interactive attribute :I: so one can call interactive program
 *    in a recipe (e.g., syncweb)
 *  - use of MKSHELL to configure which shell to use
 *    (original mk was doing that but only if MKSHELL was set in an mkfile)
 *  - simplifications by not supporting the features mentioned above
 *  - be more relaxing on date (or TODO use nanosec); if equal time then ok
 *    (modern machines can generate the .o and a.out in the same second)
 *  - generate error when no mkfile
 *  - TODO warn at least when we think shprint might be wrong
 *  - better error when found cycle, show full trace!
 *  - TODO better error message when error in recipe, right now
 *    I get the error at the beginning and a trailing of regular shprint
 *    (but more Plan 9's style, so at least dont print the rest? or print
 *     also message at the end that something went wrong)
 *  - TODO a luisa mode, more synthetic, just DONE
 *  - TODO: like in buck, show all processors and IDLE or active (GUI?)
 * 
 * Internal improvements (IMHO):
 *  - different approach to parsing. Separate more clearly lexing, parsing,
 *    and evaluating, so avoid duplicate work like handling quoted characters
 *    or percent at many places.
 *  - less use of globals, pass them around
 * 
 * todo:
 *  - look at source code of omake? and mk-in-go?
 *  - store all output of children process and output only
 *    command that generates error! luisa will be happier :) no more long
 *    command line scrolling
 *    (and no interleaving of command output far away from originator,
 *    as in ninja)
 *  - output only a short version of the command instead of full shprint
 *    like ocamlc ... foo.ml (as in Linux Makefiles and ninja),
 *    or foo.cmo <- foo.ml, foo.byte <- foo.cmo bar.cmo ...
 *  - some flags (-a, etc)
 *  - improve speed:
 *    * -u
 *    * use nproc for environment
 *    * profile mk.byte
 *  - a -profile, a la pad, so see times in parsing, in graph exploration,
 *    etc.
 *  - a -dump and -restart, as in emacs -dump and undump(), see the comments in 
 *    https://news.ycombinator.com/item?id=13073566
 *  - we could reduce a bit the size of the code by reusing cpp!
 *    #include is equivalent of <, #define is equivalent of variable definition
 *    but for variable we would still need the dynamic binding of
 *    $target, $prereq, so maybe not good to provide an extra and different
 *    #define mechanism and syntax for using variables/constants.
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)

(* see the .mli for why those caps are needed *)
type caps = < Cap.fork; Cap.exec; Cap.env; Cap.argv; Cap.chdir >

let usage =
  "usage: mk [-f file] [options] [targets ...]"

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(* to test the different mk components *)
let do_action caps s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        Logs.info (fun m -> m "processing %s" file);
        let instrs = Parse.parse (Fpath.v file) in
        Console.print caps (spf "%s" (Ast.show_instrs instrs))
      )
  | "-test_eval" ->
      xs |> List.iter (fun file ->
        Logs.info (fun m -> m "processing %s" file);
        let env = Env.initenv caps in
        let instrs = Parse.parse (Fpath.v file) in
        let _rules, env = Eval.eval caps env (ref []) instrs in
        Env.dump_env env;
        ()
      )
  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let build_target (caps : caps) (env : Env.t) (rules : Rules.rules) (target : string) : unit =

   let root = Graph.build_graph target rules in

   (* could do that after the checks *)
   if !Flags.dump_graph 
   then Graph.dump_graph root;
   
   let ever_did = ref false in

   while root.G.state = G.NotMade do
     let did = ref false in

     (* may call internally Scheduler.run to schedule jobs and may
      * raise some Failure (e.g., "don't know how to make xxx")
      *)
     Outofdate.work caps env root did;

     if !did 
     then ever_did := true
     else 
       (* no work possible, let's wait for a job process to finish *)
       if !Scheduler.nrunning > 0
       then Scheduler.waitup caps ()
       (* else: impossible? *)
   done;

   (* bugfix: root can be BeingMade in which case we need to wait *)
   while !Scheduler.nrunning > 0 do
     Scheduler.waitup caps ();
   done;
   
   if not !ever_did
   then print_string (spf "mk: '%s' is already up to date\n" root.G.name)


let build_targets (caps : caps) (infile : Fpath.t) (targets : string list ref) (vars : (string*string) list) : unit =

    (* initialisation *)
    let env = Env.initenv caps in
    vars |> List.iter (fun (var, value) ->
     (* stricter: we do not allow list of strings for value for command-line
      * vars, but anyway I'm not sure how they could be parsed by Arg
      *)
      Env.add_var env var [value];
      Hashtbl.add env.Env.vars_commandline var true;
    );
    
    if !Flags.debugger then begin
      CapSys.chdir caps (Filename.dirname !!infile);
      Env.add_var env "objtype" ["386"]
    end;

    (* parsing (and evaluating) *)
    let instrs = Parse.parse infile in

    if !Flags.dump_ast
    then Ast.dump_ast instrs;

    (* can modify targets and use first targets in file if none provided *)
    let rules, env = Eval.eval caps env targets instrs in

    if !Flags.dump_env
    then Env.dump_env env;
    
    (* building *)
    if !targets = []
    then failwith "nothing to mk";

    (* less: build shellenv here ?*)
    !targets |> List.rev |> List.iter (fun target ->
      build_target caps env rules target
    )
    (* less: profiling*)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps: <caps; Cap.stdout; ..>) (argv : string array) : Exit.t =
  let infile  = ref "mkfile" in
  let targets = ref [] in
  let vars = ref [] in

  (* for debugging *)
  let level = ref (Some Logs.Warning) in
  let action = ref "" in
  let backtrace = ref false in

  let options = [

    (* less: maybe should do a chdir (Dirname infile) *)
    "-f", Arg.Set_string infile,
    " <file> use file instead of mkfile";
    
    "-e", Arg.Set Flags.explain_mode,
    " explain mode";
    "-n", Arg.Set Flags.dry_mode,
    " dry mode";
    (* less: -a, etc *)
    "-strict", Arg.Set Flags.strict_mode,
    " strict mode";

    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";
    "-test_eval", Arg.Unit (fun () -> action := "-test_eval"), " ";

    (* pad: I added that *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    "-dump_env", Arg.Set Flags.dump_env,
    " dump the environment";
    "-dump_graph", Arg.Set Flags.dump_graph,
    " dump the generated graph (in graphviz dot format)";
    "-dump_jobs", Arg.Set Flags.dump_jobs,
    " ";

    (* TODO: move in a CLI_common.ml *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    "-debug", Arg.Unit (fun () -> 
      level := Some Logs.Debug;
      Flags.explain_mode := true;
    ),
    " trace the main functions";
    
    "-debugger", Arg.Set Flags.debugger,
    " ";
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
  ]
  in
  (* old: was Arg.parse but we want explicit argv control *)
  (try
    Arg.parse_argv argv (Arg.align options) (fun t -> 
      match t with
      | _ when t =~ "^\\(.*\\)=\\(.*\\)$" ->
        let (var, value) = Regexp_.matched2 t in
        vars := (var, value)::!vars;
      | _ ->
        targets := t :: !targets
      ) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );

  Logs.set_level !level;
  Logs.info (fun m -> m "ran from %s" (Sys.getcwd ()));

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action caps !action (List.rev !targets); 
    raise (Exit.ExitCode 0)
  end;

  (* Let's go! *)
  try 
    build_targets (caps :> caps ) (Fpath.v !infile) targets !vars;
    Exit.OK
  with exn ->
    if !backtrace || !Flags.debugger
    then raise exn
    else 
      (match exn with
      (* lots of the mk errors are reported using failwith (e.g., "don't know
       * how to make xxx")
       *)
      | Failure s -> 
          (* useful to indicate that error comes from mk, not subprocess *)
          Logs.err (fun m -> m "mk: %s" s);
          (* need to wait for other children before exiting, otherwise
           * could get corrupted incomplete object files.
           *)
          while !Scheduler.nrunning > 0 do
              try 
                (* todo: if dump_jobs, print pid we wait and its recipe *)
                Unix.wait () |> ignore;
                decr Scheduler.nrunning
              with Unix.Unix_error (error, _str1, _str2) ->
                failwith (spf "%s" (Unix.error_message error))
          done;
          Exit.Code 1
      | _ -> raise exn
      )
