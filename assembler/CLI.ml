(* Copyright 2015, 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Regexp_.Operators

(* for field access for ocaml-light *)
open Preprocessor

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5a, the Plan 9 ARM assembler.
 *
 * Main limitations compared to 5a:
 *  - no multiple files processing in parallel 
 *    (not the place, use xargs)
 *  - no unicode support? or can ocamllex in ocaml-light do unicode?
 * 
 * todo:
 *  - advanced instructions: floats, MULL, coprocessor, PSR, etc
 *  - handle the instructions used in the kernel
 * later:
 *  - look at the 5a Go sources in the Golang source, maybe ideas to steal?
 *  - make it a multi-archi assembler by following
 *    the new design by Rob Pike of Go assembler to factorize things
 *    (see https://www.youtube.com/watch?v=KINIAgRpkDA&feature=youtu.be )
 *    (=~ 2 tables, register string -> code, and opcode string -> code
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(* Need: see .mli *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >

let thechar = '5'
let thestring = "arm"

let usage = 
  spf "usage: %ca [-options] file.s" thechar

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(* Will generate outfile as a side effect *)
let assemble5 (caps: < caps; .. >) dump (conf : Preprocessor.conf) (infile : Fpath.t) (outfile : Fpath.t) : unit =
  let prog = Parse_asm5.parse caps conf infile in
  let prog = Resolve_labels5.resolve prog in
  if dump 
  then prog |> Meta_ast_asm5.vof_program |> OCaml.string_of_v |> (fun s -> 
        Logs.app (fun m -> m "AST = %s" s));
  outfile |> FS.with_open_out caps (fun chan ->
      Object_code5.save (prog, !Location_cpp.history) chan
  )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps: <caps; ..>) (argv: string array) : Exit.t =
  (* alt: Fpath.t option ref *)
  let infile  = ref "" in
  let outfile = ref "" in

  let level = ref (Some Logs.Warning) in
  let dump    = ref false in
  (* for debugging *)
  let backtrace = ref false in

  (* for cpp *)
  let include_paths : Fpath.t list ref = ref [] in
  let macro_defs = ref [] in

  let options = [
    "-o", Arg.Set_string outfile,
    " <file> output file";

    (* dup: same in compiler/CLI.ml *)
    "-D", Arg.String (fun s ->
      let (var, val_) = 
        if s =~ "\\(.*\\)=\\(.*\\)"
        then Regexp_.matched2 s
        else (s, "1")
      in
      macro_defs := (var, val_)::!macro_defs
    ), " <name=def> (or just <name>) define the name to the preprocessor";
    "-I", Arg.String (fun s ->
      include_paths := Fpath.v s::!include_paths
    ), " <dir> add dir as a path to look for '#include <file>' files";

    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " guess what";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";

    (* pad: I added that *)
    "-dump", Arg.Set dump,
    " dump the parsed AST";
    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
  ]
  in
  (try
    Arg.parse_argv argv (Arg.align options) (fun f -> 
     if !infile <> ""
     then failwith "already specified an input file";
     infile := f;
   ) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "assembler ran from %s" (Sys.getcwd()));

  if !infile = ""
  then begin Arg.usage options usage; raise (Exit.ExitCode 1); end;

  let outfile : Fpath.t = 
    (if !outfile = ""
    then
      let b = Filename.basename !infile in
      if b =~ "\\(.*\\)\\.s"
      then Regexp_.matched1 b ^ (spf ".%c" thechar)
      else (b ^ (spf ".%c" thechar))
    else !outfile
    ) |> Fpath.v
  in

  (* dup: same in compiler/main.ml *)
  let system_paths : Fpath.t list =
    (try CapSys.getenv caps "INCLUDE" |> Str.split (Str.regexp "[ \t]+")
     with Not_found ->
       [spf "/%s/include" thestring; "/sys/include";]
    ) |> Fpath_.of_strings
  in
  let conf : Preprocessor.conf = {
    defs = !macro_defs;
    (* this order? *)
    paths = system_paths @ List.rev !include_paths;
    dir_source_file = Fpath.v (Filename.dirname !infile);
  }
  in

  try 
    (* main call *)
    (* TODO: create chan from infile outfile instead so no need
     * pass heavy capabilities (Cap.open_in, Cap.open_out)
     *)
    assemble5 caps !dump conf (Fpath.v !infile) outfile;
    Exit.OK
  with exn ->
    if !backtrace
    then raise exn
    else 
      (match exn with
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          Logs.err (fun m -> m "%s:%d %s" !!file line s);
          Exit.Code 1
      | _ -> raise exn
      )

