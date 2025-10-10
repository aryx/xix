(* Copyright 2015, 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Regexp_.Operators

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
 *  - look at the 5a Go sources in the Golang source, maybe ideas to steal?
 *  - advanced instructions: floats, MULL, coprocessor, PSR, etc
 *  - make it a multi-archi assembler by following
 *    the new design by Rob Pike of Go assembler to factorize things
 *    (see https://www.youtube.com/watch?v=KINIAgRpkDA&feature=youtu.be )
 *    (=~ 2 tables, register string -> code, and opcode string -> code
 *  - instructions used in kernel
 *)

let thechar = '5'
let thestring = "arm"

let usage = 
  spf "usage: %ca [-options] file.s" thechar

let assemble5 dump (defs, paths) (infile : Fpath.t) outfile =
  let prog = Parse_asm5.parse (defs, paths) infile in
  let prog = Resolve_labels5.resolve prog in
  if dump 
  then prog |> Meta_ast_asm5.vof_program |> OCaml.string_of_v |> (fun s -> 
        Logs.app (fun m -> m "AST = %s" s));
  Object_code5.save (prog, !Location_cpp.history) outfile


let main (caps: Cap.all_caps) =
  let infile  = ref "" in
  let outfile = ref "" in

  let dump    = ref false in

  (* for cpp *)
  let include_paths = ref [] in
  let macro_defs = ref [] in

  (* for debugging *)
  let backtrace = ref false in

  let options = [
    "-o", Arg.Set_string outfile,
    " <file> output file";

    (* dup: same in compiler/main.ml *)
    "-D", Arg.String (fun s ->
      let (var, val_) = 
        if s =~ "\\(.*\\)=\\(.*\\)"
        then Regexp_.matched2 s
        else (s, "1")
      in
      macro_defs := (var, val_)::!macro_defs
    ), " <name=def> (or just <name>) define the name to the preprocessor";
    "-I", Arg.String (fun s ->
      include_paths := s::!include_paths
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
  then begin Arg.usage options usage; CapStdlib.exit caps (-1); end;

  let outfile = 
    if !outfile = ""
    then
      let b = Filename.basename !infile in
      if b =~ "\\(.*\\)\\.s"
      then Regexp_.matched1 b ^ (spf ".%c" thechar)
      else b ^ (spf ".%c" thechar)
    else !outfile
  in

  (* dup: same in compiler/main.ml *)
  let system_paths =
    (try CapSys.getenv caps "INCLUDE" |> Str.split (Str.regexp "[ \t]+")
     with Not_found ->
       [spf "/%s/include" thestring; "/sys/include";]
    )
  in
  let include_paths = system_paths @ (List.rev !include_paths) in
  let dir = Filename.dirname !infile in

  try 
    (* main call *)
    assemble5 !dump (!macro_defs, (dir, include_paths)) (Fpath.v !infile) outfile
  with  exn ->
    if !backtrace
    then raise exn
    else 
      (match exn with
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          Logs.err (fun m -> m "%s:%d %s" !!file line s);
          CapStdlib.exit caps (-1);
      | _ -> raise exn
      )

let _ = 
  Cap.main main
