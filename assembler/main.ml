(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5a, the Plan9 ARM assembler.
 *
 * Main limitations compared to 5a:
 *  - no embedded macro processor but handle at least the #line directive
 *    (better to factorize, use external cpp)
 *  - no multiple files processing in parallel 
 *    (not the place, use xargs)
 *  - no unicode support
 * 
 * todo:
 *  - advanced instructions: floats, MULL, coprocessor, psr, etc
 *  - better Lexer_asm.error() and Parser_asm.error() using lines_directives?
 *    (actually prfile() was buggy. gcc -I have #line for included file? 
 *     can reconstruct tree?)
 *)

let thechar = '5'
let usage = 
  spf "usage: %ca [-options] file.s" thechar

let assemble5 dump infile outfile =
  let prog = Parse_asm5.parse infile in
  let prog = Resolve_labels5.resolve prog in
  if dump 
  then prog |> Meta_ast_asm5.vof_program |> Ocaml.string_of_v |> Common.pr2;
  Object_code5.save (prog, infile) outfile


let main () =
  let infile  = ref "" in
  let outfile = ref "" in
  let dump    = ref false in

  let options = [
    "-o", Arg.Set_string outfile,
    " <file> output file";

    (* pad: I added that *)
    "-dump", Arg.Set dump,
    " dump the parsed AST";
  ]
  in
  Arg.parse options
   (fun f -> 
     if !infile <> ""
     then failwith "already specified an input file";
     infile := f;
   )
   usage;

  if !infile = ""
  then begin Arg.usage options usage; exit (-1); end;
  let outfile = 
    if !outfile = ""
    then
      let b = Filename.basename !infile in
      if b =~ "\\(.*\\)\\.s"
      then Common.matched1 b ^ (spf ".%c" thechar)
      else b ^ (spf ".%c" thechar)
    else !outfile
  in

  (* main call *)
  assemble5 !dump !infile outfile
  

let _ = 
  main ()
