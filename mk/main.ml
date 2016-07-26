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
 *  - no fancy variable names with space or other special symbols in ${}
 *    (no regular PL allows this anyway)
 *  - only one -f is supported, not an array of up to 256 mkfiles
 *    (who uses that anyway?)
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
 *  - different approach to parsing. Separate more clearly lexing, parsing,
 *    and evaluating, so avoid duplicate work like handling quoted characters
 *    or percent at many places.
 *  - generate error when no mkfile
 *  - TODO warn at least when guess that shprint might be wrong
 *  - TODO better error when found cycle, show full trace!
 * 
 * Todo:
 *  - xx=yyy overriding
 *  - ${x:%==%}
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



let main () =
  let infile  = ref "mkfile" in
  let targets = ref [] in
  let dump    = ref false in
  

  let options = [

    "-f", Arg.Set_string infile,
    " <file> use file instead of mkfile";

    (* less: -n, -a, etc *)

    (* pad: I added that *)
    "-dump", Arg.Set dump,
    " dump the parsed AST";
  ]
  in
  (* less: handle xx=yy *)
  Arg.parse options
   (fun t -> 
     targets := t :: !targets
   )
   usage;
  
  (* initialisation *)
  let env = Env.initenv() in

  (* parsing (and evaluating) *)

  let instrs = Parse.parse !infile in
  if !dump then instrs |> List.iter pr2_gen;

  let rules, env = Eval.eval env targets instrs in

  (* building *)
  if !targets = []
  then failwith "mk: nothing to mk";

  (* less: build shellenv here ?*)
  !targets |> List.rev |> List.iter (fun target ->
    (* can call Graph.graph here ? outside of Mk.build_target? *)
    build_target env rules target
  );
    (* less: profiling*)
  ()


let _ = 
  main ()
