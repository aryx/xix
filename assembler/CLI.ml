(* Copyright 2015, 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Regexp_.Operators

(* for field access for ocaml-light *)
open Preprocessor

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5a/va, the Plan 9 ARM/MIPS assemblers.
 *
 * Main limitations compared to 5a/va/...:
 *  - no multiple files processing in parallel 
 *    (not the place, use xargs)
 *  - no unicode support? or can ocamllex in ocaml-light do unicode?
 *
 * better than 5a/va/...:
 *  - far greater code reuse across all assemblers thanks to:
 *    * Lexer_asm.mll factorization
 *    * Ast_asm.ml and 'instr polymorphic type
 *    * use of simple marshalling instead of adhoc object format
 *      (which were slightly different in each arch)
 *    * separate AST generation from resolving which allows to factorize
 *      checks of redefinition and the resolution of labels
 *    * generalize more code in macroprocessor/
 *    * use simple marshal again for cpp line history
 * 
 * todo:
 *  - port remaining assembler: ia, 7a, 8a, 6a (increasing difficulty)
 * later:
 *  - look at the 5a Go sources in the Golang source, maybe ideas to steal?
 *  - follow the new design by Rob Pike of Go assembler to factorize things
 *    (see https://www.youtube.com/watch?v=KINIAgRpkDA&feature=youtu.be )
 *    (=~ 2 tables, register string -> code, and opcode string -> code)
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(* Need: see .mli *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >

let dump_ast = ref false

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let assemble5 (caps: < Cap.open_in; .. >) (conf : Preprocessor.conf) (infile : Fpath.t) : Ast_asm5.program =
  let prog = Parse_asm5.parse caps conf infile in
  let prog = Resolve_labels.resolve Ast_asm5.branch_opd_of_instr prog in
  if !dump_ast 
  then prog |> Meta_ast_asm5.vof_program |> OCaml.string_of_v |> (fun s -> 
        Logs.app (fun m -> m "AST = %s" s));
  prog

let assemblev (caps: < Cap.open_in; .. >) (conf : Preprocessor.conf) (infile : Fpath.t) : Ast_asmv.program =
  let prog = Parse_asmv.parse caps conf infile in
  let prog = Resolve_labels.resolve Ast_asmv.branch_opd_of_instr prog in
  if !dump_ast 
  then Logs.app (fun m -> m "AST = %s" (Ast_asmv.show_program prog));
  prog


(* Will modify chan as a side effect *)
let assemble (caps: < Cap.open_in; .. >) (conf : Preprocessor.conf) (arch: Arch.t) (infile : Fpath.t) (chan : Chan.o) : unit =
  match arch with
  | Arch.Arm -> 
      let prog = assemble5 caps conf infile in
      Object_file.save prog chan
  | Arch.Mips -> 
      let prog = assemblev caps conf infile in
      Object_file.save prog chan
  | _ -> 
   failwith (spf "TODO: arch not supported yet: %s" (Arch.thestring arch))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps: <caps; ..>) (argv: string array) : Exit.t =
  let arch = 
    (* alt: use Arch.arch_of_char argv.(0).(1) *)
    match Filename.basename argv.(0) with
    | "o5a" -> Arch.Arm
    | "ova" -> Arch.Mips
    | s -> failwith (spf "arch could not be detected from argv0 %s" s)
  in

  let thechar = Arch.thechar arch in
  let thestring = Arch.thestring arch in

  let usage = 
    spf "usage: %s [-options] file.s" argv.(0)
  in

  (* alt: Fpath.t option ref *)
  let infile  = ref "" in
  let outfile = ref "" in

  let level = ref (Some Logs.Warning) in
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
    "-dump_ast", Arg.Set dump_ast,
    " dump the parsed AST";
    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
  ] |> Arg.align
  in
  (try
    Arg.parse_argv argv options (fun f -> 
     if !infile <> ""
     then failwith "already specified an input file";
     infile := f;
   ) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "assembler ran from %s with arch %s" 
        (Sys.getcwd()) thestring);

  if !infile = ""
  then begin Arg.usage options usage; raise (Exit.ExitCode 1); end;

  let outfile : Fpath.t = 
    (if !outfile = ""
    then
      let b = Filename.basename !infile in
      if b =~ "\\(.*\\)\\.s"
      then Regexp_.matched1 b ^ (spf ".o%c" thechar)
      else (b ^ (spf ".o%c" thechar))
    else !outfile
    ) |> Fpath.v
  in

  (* dup: same in compiler/CLI.ml *)
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
    (* can't create chan from infile to avoid passing Cap.open_in because
     * with cpp we need to open other files.
     *)
    outfile |> FS.with_open_out caps (fun chan ->
        assemble caps conf arch (Fpath.v !infile) chan
    );
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

