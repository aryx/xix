(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

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
 *    (I never saw mkfiles using it)
 *  - only one -f is supported, not an array of up to 256 mkfiles
 *    (who uses that? maybe to have mk -f varfile -f mkfile)
 *  - no sequential vs parallel mode, and no parallel for multi targets
 *    (most of the time you give just one target anyway)
 *  - disallow :=< in more context
 *    (confusing for reader anyway)
 *  - disallow dynamic assignements like $X=1
 *    (who uses that?)
 *  - no opti like missing intermediate (mk -i)
 *    (I barely understand the algorithm anyway)
 *  - no vacuous check
 *    (I barely understand the algorithm anyway)
 *  - no unicode support
 * 
 * Improvements (IMHO):
 *  - TODO be more relaxing on date (or use nanosec); if equal time then ok
 *    (modern machines can generate the .o and a.out in the same second)
 *  - generate error when no mkfile
 *  - TODO warn at least when guess that shprint might be wrong
 *  - TODO better error when found cycle, show full trace!
 *  - TODO better error message when error in recipe, right now
 *    I get the error at the beginning and a trailing of regular shprint
 *
 * Internal improvements (IMHO):
 *  - different approach to parsing. Separate more clearly lexing, parsing,
 *    and evaluating, so avoid duplicate work like handling quoted characters
 *    or percent at many places.
 * 
 * Todo:
 *  - xx=yyy overriding
 *  - some flags (-a, -e, etc)
 *  - recursive mk? dynamic mkfile?
 *  - look at source code of omake? and mk-in-go?
 *)

let usage =
  "usage: mk [-f file] [options] [targets ...]"

(*val build_target: 
  Env.t -> Rules.t -> string (* target *) -> unit
*)
let build_target env rules target =
  raise Todo


(* to test the different mk components *)
let do_action s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        pr2 (spf "processing %s" file);
        let _ast = Parse.parse file in
        ()
      )
  | _ -> failwith ("action not supported: " ^ s)


let main () =
  let infile  = ref "mkfile" in
  let targets = ref [] in
  let action = ref "" in

  let options = [

    "-f", Arg.Set_string infile,
    " <file> use file instead of mkfile";

    (* less: -n, -a, etc *)

    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"),
    " ";

    (* pad: I added that *)
    "-debug_lexer", Arg.Set Flags.debug_lexer,
    " dump the tokens as they are generated";
    "-debug_ast", Arg.Set Flags.debug_ast,
    " dump the parsed AST";
  ]
  in
  (* less: handle xx=yy *)
  Arg.parse (Arg.align options) (fun t -> targets := t :: !targets) usage;

  (* initialisation *)
  let env = Env.initenv() in

  (* to test and debug components of mk *)
  if !action <> "" then begin do_action !action (List.rev !targets); exit 0 end;

  (* parsing (and evaluating) *)
  let instrs = Parse.parse !infile in
  let rules, env = Eval.eval env targets instrs in

  (* building *)
  if !targets = []
  then failwith "mk: nothing to mk";

  (* less: build shellenv here ?*)
  !targets |> List.rev |> List.iter (fun target ->
    build_target env rules target
  );
    (* less: profiling*)
  ()


let _ = 
  main ()
