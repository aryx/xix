(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Regexp_.Operators
(* for record-building for ocaml-light *)
open Preprocessor

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5c, the Plan 9 C compiler for ARM.
 *
 * Main limitations compared to 5c/vc/...:
 *  - no unicode support
 *  - can not compile multiple files at the same time
 *    (but you should use mk anyway)
 *  - can not compile from stdin
 *    (but who uses that?)
 *  - no -. to remove auto search for header in current directory
 *    (but who uses that?)
 *  - no error recovery, we stop at the first error (except in check.ml)
 *    (but compiler now fast enough and errors have a domino effect anyway)
 *  - no support for certain kencc extensions: 
 *     * STILL? unnamed structure element
 *       (confusing anyway, and annoying when porting code to gcc/clang)
 *     * typestr
 *       (seems dead)
 *  - no support for certain C features:
 *     * enum float
 *       (who uses that?)
 *
 * stricter:
 *  - stricter for grammar (see parser.mly), for instance force a specific
 *    order between the sign, qualifier, and type.
 *  - disallow implicit declarations of functions
 *  - stricter for typechecking (see typecheck.ml), for instance
 *    we do not support void* conversions (5c -V), and we use name
 *    equality for typechecking structs, not field equality.
 *    we also do not automatically transform 0 in nil; I force to write
 *    nil
 * 
 * improvements:
 *  - we forbid more constructs: 
 *     * typedef and initializers, 
 *     * typedef function definitions, 
 *     * three dots parameter in the middle, 
 *     * far more (see tests/)
 *  - better error location (no use of vague nearln) and
 *    better error messages (a la clang)
 * 
 * todo:
 *  - finish enough to at least handle helloc.c (basic function calls, basic
 *    types)
 *  - safe-linking support
 *  - debugger support
 *  - profiler support
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(* Need: see .mli *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let do_action (caps: < caps; .. >) thestring s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        Logs.info (fun m -> m "processing %s" file);
        let conf : Preprocessor.conf = {
          defs = [];
          paths = [spf "/%s/include" thestring; "/sys/include";] |> Fpath_.of_strings;
          dir_source_file = Fpath.v ".";
        }
        in
        try 
          let _ = Parse.parse caps conf (Fpath.v file) in
          ()
        with Location_cpp.Error (s, loc) ->
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          failwith (spf "%s:%d %s" !!file line s)
      )

  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Main algorithms *)
(*****************************************************************************)

(* TODO: move somewhere else, rename ids_structs_funcs? or use
 * record with fields!
 *)
type frontend_result = 
  (Ast.fullname, Typecheck.idinfo) Hashtbl.t *
  (Ast.fullname, Type.struct_kind * Type.structdef) Hashtbl.t *
  Ast.func_def list

let frontend (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf) (infile : Fpath.t) : frontend_result =

  let ast = Parse.parse caps conf infile in

  (* debug *)
  if !Flags.dump_ast
  then Logs.app (fun m -> m "%s" (Dumper_.s_of_any (Ast.Program ast)));

  (* use/def checking, unused entity, redefinitions, etc. *)
  Check.check_program ast;
  (* typedef expansion, type and storage resolution, etc. *)
  let (ids, structs, funcs) = 
    Typecheck.check_and_annotate_program ast 
  in
  
  (* debug *)
  if !Flags.dump_typed_ast
  then begin 
    ids |> Hashtbl.iter (fun k v ->
      match v.Typecheck.sto with
      | Storage.Global | Storage.Static ->
        Logs.app (fun m -> m "%s" (Ast.unwrap k));
        Logs.app (fun m -> m "%s" (Dumper_.s_of_any (Ast.FinalType v.Typecheck.typ)));
      | _ -> ()
    );
    funcs |> List.iter (fun func ->
      Logs.app (fun m -> m "%s" (Dumper_.s_of_any_with_types (Ast.Toplevel (Ast.FuncDef func))))
    );
  end;

  ids, structs, funcs

let backend5 (ids, structs, funcs)  (chan : Chan.o) : unit =
  (* todo: Rewrite.rewrite *)
  let (asm, _locs) = Codegen5.codegen (ids, structs, funcs) in

  if !Flags.dump_asm
  then begin
    let pc = ref 0 in
    asm |> List.iter (fun (instr, _loc) ->
      (* less: use a assembly pretty printer instead? easier to debug? 5c -S *)
      let v = Meta_ast_asm5.vof_line instr in
      Logs.app (fun m -> m  "%2d: %s" !pc (OCaml.string_of_v v));
      incr pc;
    );
  end;
  Object_file.save Arch.Arm (asm, !Location_cpp.history) chan

let compile5 (caps : < Cap.open_in; ..>) (conf : Preprocessor.conf) (infile : Fpath.t)
  (outfile : Chan.o) =
  let (ids, structs, funcs) = 
    frontend caps conf infile in
  backend5 (ids, structs, funcs) outfile

let compile (caps : < Cap.open_in; ..>) (conf : Preprocessor.conf) (arch : Arch.t) (infile : Fpath.t) (outfile : Chan.o) : unit =
  match arch with
  | Arch.Arm -> compile5 caps conf infile outfile
  | _ -> 
   failwith (spf "TODO: arch not supported yet: %s" (Arch.thestring arch))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =

  let arch = 
    match Filename.basename argv.(0) with
    | "o5c" -> Arch.Arm
    | "ovc" -> Arch.Mips
    | s -> failwith (spf "arch could not detected from argv0 %s" s)
  in

  let thechar = Arch.thechar arch in
  let thestring = Arch.thestring arch in

  let usage = 
    spf "usage: %s [-options] file.c" argv.(0)
  in

  (* in *)
  let args = ref [] in
  (* out *)
  let outfile = ref "" in

  (* for cpp *)
  let include_paths : Fpath.t list ref = ref [] in
  let macro_defs = ref [] in

  (* Ansi Posix Environment for plan9 *)
  let ape = ref false in 

  let level = ref (Some Logs.Warning) in
  (* for debugging *)
  let action = ref "" in
  let backtrace = ref false in

  let options = [
    "-o", Arg.Set_string outfile,
    " <file> place output (an object) in file";

    "-D", Arg.String (fun s ->
      let (var, val_) = 
        if s =~ "\\(.*\\)=\\(.*\\)"
        then Regexp_.matched2 s
        else (s, "1")
      in
      macro_defs := (var, val_)::!macro_defs
    ), " <name=def> (or just <name>) define name for preprocessor";
    "-I", Arg.String (fun s ->
      include_paths := Fpath.v s::!include_paths
    ), " <dir> add dir as a path to look for '#include <file>' files";
    "-ape", Arg.Set ape,
    " ";

    "-w", Arg.Set Flags.warn,
    " enable warnings";
    "-werror", Arg.Set Flags.warnerror,
    " warnings generate error exceptions";

    "-S", Arg.Set Flags.dump_asm,
    " dump the generated assembly";

    "-e", Arg.Set Flags_cpp.debug_include, " ";
    "-f", Arg.Set Flags_cpp.debug_line, " ";
    "-m", Arg.Set Flags_cpp.debug_macros, " ";

    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " guess what";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";

    (* pad: I added long names for those options *)
    "-debug_include", Arg.Set Flags_cpp.debug_include, " ";
    "-debug_line",      Arg.Set Flags_cpp.debug_line, " ";
    "-debug_macros",    Arg.Set Flags_cpp.debug_macros, " ";

    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";

    (* pad: I added that *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    "-dump_typed_ast", Arg.Set Flags.dump_typed_ast,
    " dump the typed AST";
    "-dump_asm", Arg.Set Flags.dump_asm,
    " dump the generated assembly";

    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
  ] |> Arg.align
  in
  (try
    Arg.parse_argv argv options (fun t -> 
      args := t::!args
    ) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "assembler ran from %s" (Sys.getcwd()));

  (* less: process the old style -Dname=val and -Idir attached *)

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action caps thestring !action (List.rev !args); 
    raise (Exit.ExitCode 0 )
  end;

  try 
    (match !args, !outfile with
    | [], "" -> 
        Arg.usage options usage;
        Exit.Code 1
    | [cfile], outfile ->
        let base = Filename.basename cfile in
        let system_paths : Fpath.t list =
          (try CapSys.getenv caps "INCLUDE" |> Str.split (Str.regexp "[ \t]+")
          with Not_found ->
            [spf "/%s/include" thestring; 
             "/sys/include";
            ] |> (fun xs -> if !ape then "/sys/include/ape"::xs else xs)
          ) |> Fpath_.of_strings
        in
        
        let outfile : Fpath.t = 
          (if outfile = ""
          then
            if base =~ "\\(.*\\)\\.c"
            then Regexp_.matched1 base ^ (spf ".%c" thechar)
            else base ^ (spf ".%c" thechar)
          else outfile
          ) |> Fpath.v
        in
        let conf : Preprocessor.conf = {
          defs = !macro_defs;
          (* this order? *)
          paths = system_paths @ List.rev !include_paths;
          dir_source_file = Fpath.v (Filename.dirname cfile);
        }
        in
        outfile |> FS.with_open_out caps (fun chan ->
          compile caps conf arch (Fpath.v cfile) chan
        );
        Exit.OK
    | _ -> 
      (* stricter: *)
        failwith 
          "compiling multiple files at the same time is not supported; use mk"
    )
  with exn ->
    if !backtrace
    then raise exn
    else 
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from 5c? *)
          Error.errorexit (spf "%cc: %s" thechar s)
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          Error.errorexit (spf "%s:%d %s" !!file line s)
      (* ocaml-light: | Check.Error err | Typecheck.Error err | Eval_const.Error err | Codegen5.Error err  *)
      | Check.Error err -> Error.errorexit (Check.string_of_error err)
      | Typecheck.Error err -> Error.errorexit (Check.string_of_error err)
      | Eval_const.Error err -> Error.errorexit (Check.string_of_error err)
      | Codegen5.Error err -> Error.errorexit (Check.string_of_error err)
      | _ -> raise exn
      )
