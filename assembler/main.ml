(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5a, the plan9 ARM assembler.
 *
 * Limitations compared to 5a:
 *  - no multiple files processing in parallel (not the place, use xargs)
 * 
 * todo?:
 *  - better Lexer_asm.error() and Parser_asm.error() using lines_directives?
 *    actually prfile() was buggy
 *    gcc -I have #line for included file? can reconstruct tree?
 *)

let thechar = '5'
let usage = 
  spf "usage: %ca [-options] file.s" thechar

let assemble5 infile outfile =
  let prog = Parse_asm5.parse infile in
  let prog = Resolve_labels5.resolve prog in
  Object_code5.save (prog, infile) outfile


let main () =
  let infile = ref "" in
  let outfile = ref "" in
  let options = [
    "-o", Arg.Set_string outfile,
    " <file> output file";
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
  if !outfile = ""
  then begin 
    let b = Filename.basename !infile in
    if b =~ "\\(.*\\)\\.s"
    then outfile := Common.matched1 b ^ (spf ".%c" thechar)
    else outfile := b ^ (spf ".%c" thechar)
  end;
  assemble5 !infile !outfile
  

let _ = 
  main ()
