(*s: CLI.ml *)
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

(*s: type [[CLI.caps]] *)
(* Need:
 *  - fork/exec: obviously as we run shell commands
 *  - env: for Env.initenv() so mk recipe can access env variables.
 *    Also MKSHELL in Shell.ml and NPROC in Scheduler.ml
 *  - argv: for setting MKFLAGS also in Env.initenv()
 *  - chdir: actually needed just for -debugger, we could remove
 *)
type caps = < Cap.fork; Cap.exec; Cap.env; Cap.argv; Cap.chdir >
(*e: type [[CLI.caps]] *)

(*s: constant [[CLI.usage]] *)
let usage =
  "usage: mk [-f file] [options] [targets ...]"
(*e: constant [[CLI.usage]] *)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(*s: function [[CLI.do_action]] *)
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
(*e: function [[CLI.do_action]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(*s: function [[CLI.build_target]] *)
let build_target (caps : caps) (env : Env.t) (rules : Rules.rules) (target : string) : unit =

   let root = Graph.build_graph target rules in
   (*s: [[CLI.build_target()]] possibly dump the graph *)
   (* could do that after the checks *)
   if !Flags.dump_graph 
   then Graph.dump_graph root;
   (*e: [[CLI.build_target()]] possibly dump the graph *)
   
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
[@@profiling]
(*e: function [[CLI.build_target]] *)

(*s: function [[CLI.build_targets]] *)
let build_targets (caps : caps) (infile : Fpath.t) (targets : string list ref) (vars : (string*string) list) : unit =

    (* initialisation *)
    let env = Env.initenv caps in
    (*s: [[CLI.build_targets()]] initialize [[env]] using [[vars]] *)
    vars |> List.iter (fun (var, value) ->
     (* stricter: we do not allow list of strings for value for command-line
      * vars, but anyway I'm not sure how they could be parsed by Arg
      *)
      Env.add_var env var [value];
      Hashtbl.add env.Env.vars_commandline var true;
    );
    (*e: [[CLI.build_targets()]] initialize [[env]] using [[vars]] *)
    (*s: [[CLI.build_targets()]] if debugger set *)
    if !Flags.debugger then begin
      CapSys.chdir caps (Filename.dirname !!infile);
      Env.add_var env "objtype" ["386"]
    end;
    (*e: [[CLI.build_targets()]] if debugger set *)

    (* parsing (and evaluating) *)
    let instrs = Parse.parse infile in
    (*s: [[CLI.build_targets()]] possibly dump the AST *)
    if !Flags.dump_ast
    then Ast.dump_ast instrs;
    (*e: [[CLI.build_targets()]] possibly dump the AST *)

    (* can modify targets and use first targets in file if none provided *)
    let rules, env = Eval.eval caps env targets instrs in
    (*s: [[CLI.build_targets()]] possibly dump the environment *)
    if !Flags.dump_env
    then Env.dump_env env;
    (*e: [[CLI.build_targets()]] possibly dump the environment *)
    
    (* building *)
    if !targets = []
    then failwith "nothing to mk";
    (* less: build shellenv here ?*)
    !targets |> List.rev |> List.iter (fun target ->
      build_target caps env rules target
    )
[@@profiling]
(*e: function [[CLI.build_targets]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[CLI.main]] *)
let main (caps: <caps; Cap.stdout; ..>) (argv : string array) : Exit.t =
  let infile  = ref "mkfile" in
  let targets = ref [] in
  let vars = ref [] in
  (*s: [[CLI.main()]] debugging initializations *)
  let level = ref (Some Logs.Warning) in
  (*x: [[CLI.main()]] debugging initializations *)
  let backtrace = ref false in
  (*x: [[CLI.main()]] debugging initializations *)
  let action = ref "" in
  (*e: [[CLI.main()]] debugging initializations *)

  let options = [
    (*s: [[CLI.main()]] [[options]] elements *)
    (* less: maybe should do a chdir (Dirname infile) *)
    "-f", Arg.Set_string infile,
    " <file> use file instead of mkfile";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-e", Arg.Set Flags.explain_mode,
    " explain mode";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-n", Arg.Set Flags.dry_mode,
    " dry mode";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* TODO: move in a CLI_common.ml *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-debug", Arg.Unit (fun () -> 
      level := Some Logs.Debug;
      Flags.explain_mode := true;
    ),
    " trace the main functions";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* less: -a, etc *)
    "-strict", Arg.Set Flags.strict_mode,
    " strict mode";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-debugger", Arg.Set Flags.debugger,
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_env", Arg.Set Flags.dump_env,
    " dump the environment";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_graph", Arg.Set Flags.dump_graph,
    " dump the generated graph (in graphviz dot format)";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_jobs", Arg.Set Flags.dump_jobs,
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-test_eval", Arg.Unit (fun () -> action := "-test_eval"), " ";
    (*e: [[CLI.main()]] [[options]] elements *)
  ]
  in
  (* old: was Arg.parse but we want explicit argv control *)
  (try
    Arg.parse_argv argv (Arg.align options) (fun t -> 
    match t with
    (*s: [[CLI.main()]] modify [[vars]] when definition-like argument *)
      | _ when t =~ "^\\(.*\\)=\\(.*\\)$" ->
        let (var, value) = Regexp_.matched2 t in
        vars := (var, value)::!vars;
    (*e: [[CLI.main()]] modify [[vars]] when definition-like argument *)
    | _ -> targets := t :: !targets
    ) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  (*s: [[CLI.main()]] logging initializations *)
  Logs.set_level !level;
  Logs.info (fun m -> m "ran from %s" (Sys.getcwd ()));
  (*e: [[CLI.main()]] logging initializations *)
  (*s: [[CLI.main()]] CLI action processing *)
  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action caps !action (List.rev !targets); 
    raise (Exit.ExitCode 0)
  end;
  (*e: [[CLI.main()]] CLI action processing *)

  (* Let's go! *)
  try 
    build_targets (caps :> caps ) (Fpath.v !infile) targets !vars;
    Exit.OK
  with exn ->
    (*s: [[CLI.main()]] when [[exn]] thrown in [[build_targets()]] *)
    if !backtrace || !Flags.debugger
    then raise exn
    else 
      (match exn with
      (*s: [[CLI.main()]] when [[Failure]] [[exn]] thrown in [[build_targets()]] *)
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
      (*e: [[CLI.main()]] when [[Failure]] [[exn]] thrown in [[build_targets()]] *)
      | _ -> raise exn
      )
    (*e: [[CLI.main()]] when [[exn]] thrown in [[build_targets()]] *)
(*e: function [[CLI.main]] *)
(*e: CLI.ml *)
