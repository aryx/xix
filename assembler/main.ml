(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5a, the Plan 9 ARM assembler.
 *
 * Main limitations compared to 5a:
 *  - no multiple files processing in parallel 
 *    (not the place, use xargs)
 *  - no unicode support
 * 
 * todo:
 *  - advanced instructions: floats, MULL, coprocessor, psr, etc
 *)

let thechar = '5'
let thestring = "arm"

let usage = 
  spf "usage: %ca [-options] file.s" thechar

let assemble5 dump (defs, paths) infile outfile =
  let prog = Parse_asm5.parse (defs, paths) infile in
  let prog = Resolve_labels5.resolve prog in
  if dump 
  then prog |> Meta_ast_asm5.vof_program |> Ocaml.string_of_v |> Common.pr2;
  Object_code5.save (prog, infile) outfile


let main () =
  let infile  = ref "" in
  let outfile = ref "" in

  let dump    = ref false in

  (* for cpp *)
  let system_paths = ref [] in
  let defs = ref [] in

  (* for debugging *)
  let backtrace = ref false in

  let options = [
    "-o", Arg.Set_string outfile,
    " <file> output file";

    "-D", Arg.String (fun s ->
      let (var, val_) = 
        if s =~ "\\(.*\\)=\\(.*\\)"
        then Common.matched2 s
        else (s, "1")
      in
      defs := (var, val_)::!defs
    ), " <name=def> (or just <name>) define the name to the preprocessor";
    "-I", Arg.String (fun s ->
      system_paths := s::!system_paths
    ), " <dir> add dir as a path to look for '#include <file>' files";

    (* pad: I added that *)
    "-dump", Arg.Set dump,
    " dump the parsed AST";
    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
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

  let system_paths =
    (try Sys.getenv "INCLUDE" |> Str.split (Str.regexp "[ \t]+")
     with Not_found ->
       [spf "/%s/include" thestring; "/sys/include";]
    ) @
      !system_paths
  in
  let dir = Filename.dirname !infile in

  try 
    (* main call *)
    assemble5 !dump (!defs, (dir, system_paths)) !infile outfile
  with  exn ->
    if !backtrace
    then raise exn
    else 
      (match exn with
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          pr2 (spf "%s:%d %s" file line s);
          exit (-1);
      | _ -> raise exn
      )
  

let _ = 
  main ()
