(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module G = Graph
module R = Rules

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of mk, the Plan9 build system.
 *
 * Main limitations compared to mk:
 *  - no regexp rules
 *    (not worth it, % are good enough)
 *  - no special handling for archives
 *    (fast enough nowadays)
 *  - no :P:
 *    (it is barely documented anyway, and you can do without)
 *  - no private variables
 *    (I never saw mkfiles using it, and it complicates the parsing of '=')
 *  - no &
 *    (rarely found use, % is enough)
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
 *  - no opti like missing intermediate (mk -i)
 *    (I barely understand the algorithm anyway)
 *  - no vacuous check
 *    (I barely understand the algorithm anyway)
 *  - no unicode support
 * 
 * Improvements (IMHO):
 *  - simplifications by not supporting the features mentioned above
 *  - TODO be more relaxing on date (or use nanosec); if equal time then ok
 *    (modern machines can generate the .o and a.out in the same second)
 *  - generate error when no mkfile
 *  - TODO warn at least when we think shprint might be wrong
 *  - better error when found cycle, show full trace!
 *  - TODO better error message when error in recipe, right now
 *    I get the error at the beginning and a trailing of regular shprint
 *    (but more plan9's style, so at least dont print the rest? or print
 *     also message at the end that something went wrong)
 *
 * Internal improvements (IMHO):
 *  - different approach to parsing. Separate more clearly lexing, parsing,
 *    and evaluating, so avoid duplicate work like handling quoted characters
 *    or percent at many places.
 *  - less use of globals, pass them around
 * 
 * todo:
 *  - some flags (-a, -e, etc)
 *  - dynamic mkfile? to makeup for lack of ifdef?
 *  - recursive mk? (used by my mkfile in plan9-ml)
 *  - xx=yyy overriding and S_OVERRIDE
 *)

let usage =
  "usage: mk [-f file] [options] [targets ...]"

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(* to test the different mk components *)
let do_action s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        pr2 (spf "processing %s" file);
        let _ast = Parse.parse file in
        ()
      )
  | "-test_eval" ->
      xs |> List.iter (fun file ->
        pr2 (spf "processing %s" file);
        let env = Env.initenv() in
        let instrs = Parse.parse file in
        let _rules, _env = Eval.eval env (ref []) instrs in
        ()
      )
  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let (build_target: Env.t -> Rules.rules -> string (* target *) -> unit) =
 fun env rules target ->

   let root = Graph.build_graph target rules in
   Graph.check_cycle root;
   Graph.check_ambiguous root;

   let ever_did = ref false in

   while root.G.state = G.NotMade do
     let did = ref false in
     (* may call internally Scheduler.run to schedule jobs *)
     Outofdate.work root did;
     if !did 
     then ever_did := true
     else 
       if !ever_did 
       then Scheduler.waitup ()
   done;
   
   if not !ever_did
   then print_string (spf "mk: '%s' is already up to date\n" root.G.name)


let (build_targets: Common.filename -> string list ref -> unit) = 
 fun infile targets ->

    (* initialisation *)
    let env = Env.initenv() in
    
    if !Flags.debugger then begin
      Sys.chdir (Filename.dirname infile);
      Hashtbl.add env.Env.vars "objtype" ["386"]
    end;

    (* parsing (and evaluating) *)
    let instrs = Parse.parse infile in
    let rules, env = Eval.eval env targets instrs in
    
    (* building *)
    if !targets = []
    then failwith "mk: nothing to mk";

    (* less: build shellenv here ?*)
    !targets |> List.rev |> List.iter (fun target ->
      build_target env rules target
    )
    (* less: profiling*)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =
  let infile  = ref "mkfile" in
  let targets = ref [] in

  (* for debugging *)
  let action = ref "" in
  let backtrace = ref false in

  let options = [

    "-f", Arg.Set_string infile,
    " <file> use file instead of mkfile";
    
    "-n", Arg.Set Flags.dry_mode,
    " dry mode";
    (* less: -a, etc *)

    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";
    "-test_eval", Arg.Unit (fun () -> action := "-test_eval"), " ";

    (* pad: I added that *)
    "-debugger", Arg.Set Flags.debugger,
    " ";
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    "-dump_graph", Arg.Set Flags.dump_graph,
    " dump the generated graph";

    "-backtrace", Arg.Set backtrace,
    " dump a backtrace after an error";
  ]
  in
  Arg.parse (Arg.align options) (fun t -> 
    (* less: handle also xx=yy *)
    targets := t :: !targets
  ) usage;

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action !action (List.rev !targets); 
    exit 0 
  end;

  try 
    build_targets !infile targets
  with exn ->
    if !backtrace || !Flags.debugger
    then raise exn
    else 
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from mk, not subprocess *)
          pr2 ("mk: " ^ s);
          exit (-1)
      | _ -> raise exn
      )

let _ = 
    main ()
