(*s: CLI.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators
open Regexp_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5c/vc, the Plan 9 C compiler for ARM and MIPS.
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
 *     * unnamed structure element
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
(*s: type [[CLI.caps]] *)
(* Need:
 * - open_in: for argv derived file but also for #include'd files
 *   because 5c does its own macropreprocessing
 * - open_out for -o object file or 5.argv[0]
 * - env: for INCLUDE (for cpp)
 *)
type caps = < Cap.open_in; Cap.open_out; Cap.env >
(*e: type [[CLI.caps]] *)

let dump_rewrote_ast = ref false

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(*s: function [[CLI.do_action]] *)
let do_action (caps: < caps; .. >) thestring s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        Logs.info (fun m -> m "processing %s" file);
        let conf = Preprocessor.{
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
(*e: function [[CLI.do_action]] *)

(*****************************************************************************)
(* Main algorithms *)
(*****************************************************************************)

(*s: function [[CLI.frontend]] *)
let frontend (caps : < Cap.open_in; .. >) (conf : Preprocessor.conf)
     (infile : Fpath.t) :
    Typecheck.typed_program =

  let ast = Parse.parse caps conf infile in
  (*s: [[CLI.frontend()]] if [[dump_ast]] *)
  (* debug *)
  if !Flags.dump_ast
  then Logs.app (fun m -> m "%s" (Dumper_.s_of_any (Ast.Program ast)));
  (*e: [[CLI.frontend()]] if [[dump_ast]] *)

  (* use/def checking, unused entity, redefinitions, etc. *)
  Check.check_program ast;
  (* typedef expansion, type and storage resolution, etc. *)
  let tp : Typecheck.typed_program = 
    Typecheck.check_and_annotate_program ast 
  in
  (*s: [[CLI.frontend()]] if [[dump_typed_ast]] *)
  (* debug *)
  if !Flags.dump_typed_ast
  then begin 
    (* alt: Logs.app (fun m -> m "%s" (Typecheck.show_typed_program tp));*)
    tp.ids |> Hashtbl.iter (fun (k : Ast.fullname) (v : Typecheck.idinfo) ->
      match v.sto with
      | Storage.Global | Storage.Static | Storage.Extern ->
        Logs.app (fun m -> m "%s: %s" (Ast.unwrap k)
                              (Dumper_.s_of_any (Ast.FinalType v.typ)));
      | Storage.Param | Storage.Local -> ()
    );
    tp.funcs |> List.iter (fun func ->
      Logs.app (fun m -> m "%s" 
                (Dumper_.s_of_any_with_types (Ast.Toplevel (Ast.FuncDef func))))
    );
  end;
  (*e: [[CLI.frontend()]] if [[dump_typed_ast]] *)
  tp
(*e: function [[CLI.frontend]] *)

(*s: function [[CLI.backend5]] *)
let backend (arch : Arch.t) (tast : Typecheck.typed_program) :
    'a Ast_asm.program =

  let tast = Rewrite.rewrite tast in
  if !dump_rewrote_ast
  then Logs.app (fun m -> m "Rewrote AST: %s" 
           (Typecheck.show_typed_program tast));

  let (asm, _locs) = Codegen.codegen arch tast in
    (*s: [[CLI.backend5()]] if [[dump_asm]] *)
    (* debug *)
    if !Flags.dump_asm
    then begin
      let pc = ref 0 in
      asm |> List.iter (fun (line, _loc) ->
        (match arch with
        | Arch.Arm ->
          let instr = line in
          (* less: use a assembly pretty printer? easier to debug? 5c -S *)
          let v = Meta_ast_asm5.vof_line instr in
          Logs.app (fun m -> m  "%2d: %s" !pc (OCaml.string_of_v v));
        | Arch.Mips ->
          (* nosemgrep: do-not-use-obj-magic *)
          let instr = Obj.magic line in
          Logs.app (fun m -> m  "%2d: %s" !pc (Ast_asmv.show_line instr));
        | Arch.Riscv ->
          (* nosemgrep: do-not-use-obj-magic *)
          let instr = Obj.magic line in
          Logs.app (fun m -> m  "%2d: %s" !pc (Ast_asmi.show_line instr));
        | _ -> 
         failwith (spf "TODO: arch not supported yet: %s" (Arch.thestring arch))
        );
        incr pc;
      );
    end;
    (* nosemgrep: do-not-use-obj-magic *)
    Obj.magic asm, !Location_cpp.history
    (*e: [[CLI.backend5()]] if [[dump_asm]] *)
  
(*e: function [[CLI.backend5]] *)
(*s: function [[CLI.compile5]] *)
(*e: function [[CLI.compile5]] *)

(*s: function [[CLI.compile]] *)
let compile (caps : < Cap.open_in; ..>) (conf : Preprocessor.conf) (arch : Arch.t)
    (infile : Fpath.t) (outfile : Chan.o) :
    unit =
  let tast : Typecheck.typed_program = frontend caps conf infile in
  let asm_prog : 'a Ast_asm.program = backend arch tast in
  Object_file.save arch asm_prog outfile
(*e: function [[CLI.compile]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[CLI.main]] *)
let main (caps : <caps; Cap.stdout; Cap.stderr; ..>) (argv : string array) :
    Exit.t =
  (*s: [[CLI.main()]] set [[arch]] and [[thechar]] *)
  let arch : Arch.t = 
    match Filename.basename argv.(0) with
    | "o5c" -> Arch.Arm
    | "ovc" -> Arch.Mips
    | "oic" -> Arch.Riscv
    | s -> failwith (spf "arch could not detected from argv0 %s" s)
  in

  let thechar = Arch.thechar arch in
  let thestring = Arch.thestring arch in
  (*e: [[CLI.main()]] set [[arch]] and [[thechar]] *)
  let usage = spf "usage: %s [-options] file.c" argv.(0) in

  (* in *)
  let args = ref [] in
  (* out *)
  let outfile = ref "" in

  (*s: [[CLI.main()]] macroprocessor locals *)
  (* for cpp *)
  let include_paths : Fpath.t list ref = ref [] in
  (*x: [[CLI.main()]] macroprocessor locals *)
  let macro_defs = ref [] in
  (*e: [[CLI.main()]] macroprocessor locals *)
  (*s: [[CLI.main()]] other locals *)
  (* Ansi Posix Environment for plan9 *)
  let ape = ref false in 
  (*x: [[CLI.main()]] other locals *)
  (* for debugging *)
  let action = ref "" in
  (*x: [[CLI.main()]] other locals *)
  let level = ref (Some Logs.Warning) in
  (*x: [[CLI.main()]] other locals *)
  let backtrace = ref false in
  (*e: [[CLI.main()]] other locals *)

  let options = [
    (*s: [[CLI.main()]] [[options]] elements *)
    "-o", Arg.Set_string outfile,
    " <file> place output (an object) in file";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-w", Arg.Set Flags.warn,
    " enable warnings";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-werror", Arg.Set Flags.warnerror,
    " warnings generate error exceptions";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-ape", Arg.Set ape,
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-I", Arg.String (fun s ->
      include_paths := Fpath.v s::!include_paths
    ), " <dir> add dir as a path to look for '#include <file>' files";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-D", Arg.String (fun s ->
      let (var, val_) = 
        if s =~ "\\(.*\\)=\\(.*\\)"
        then Regexp_.matched2 s
        else (s, "1")
      in
      macro_defs := (var, val_)::!macro_defs
    ), " <name=def> (or just <name>) define name for preprocessor";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-dump_tokens", Arg.Set Flags.dump_tokens,
    " dump the tokens as they are generated";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_ast", Arg.Set Flags.dump_ast,
    " dump the parsed AST";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_typed_ast", Arg.Set Flags.dump_typed_ast,
    " dump the typed AST";
    "-dump_rewrote_ast", Arg.Set dump_rewrote_ast,
    " dump the typed AST";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-dump_asm", Arg.Set Flags.dump_asm,
    " dump the generated assembly";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-test_parser", Arg.Unit (fun () -> action := "-test_parser"), " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " guess what";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-e", Arg.Set Flags_cpp.debug_include, " ";
    "-f", Arg.Set Flags_cpp.debug_line, " ";
    "-m", Arg.Set Flags_cpp.debug_macros, " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added long names for those options *)
    "-debug_include", Arg.Set Flags_cpp.debug_include, " ";
    "-debug_line",      Arg.Set Flags_cpp.debug_line, " ";
    "-debug_macros",    Arg.Set Flags_cpp.debug_macros, " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-S", Arg.Set Flags.dump_asm,
    " dump the generated assembly";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: I added that *)
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
    (*e: [[CLI.main()]] [[options]] elements *)
  ] |> Arg.align
  in
  (*s: [[CLI.main()]] parse [[argv]] and set [[args]] and flags *)
  (* This may raise ExitCode *)
  Arg_.parse_argv caps argv options (fun t -> args := t::!args) usage;
  (*e: [[CLI.main()]] parse [[argv]] and set [[args]] and flags *)
  (*s: [[CLI.main()]] logging setup *)
  Logs_.setup !level ();
  Logs.info (fun m -> m "assembler ran from %s" (Sys.getcwd()));
  (*e: [[CLI.main()]] logging setup *)
  (* less: process the old style -Dname=val and -Idir attached *)
  (*s: [[CLI.main()]] [[action]] management *)
  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action caps thestring !action (List.rev !args); 
    raise (Exit.ExitCode 0 )
  end;
  (*e: [[CLI.main()]] [[action]] management *)

  try 
    (*s: [[CLI.main()]] matching [[args]] and [[outfile]] *)
    (match !args, !outfile with
    | [], "" -> 
        Arg.usage options usage;
        Exit.Code 1
    | [cfile], outstr ->
        (*s: [[CLI.main()]] main code path with [[cfile]] and [[outfile]] *)
        let outfile : Fpath.t = 
          (*s: [[CLI.main()]] main code path, define [[outfile]] *)
          let base = Filename.basename cfile in
          (if outstr = ""
          then begin
            let res = 
              if base =~ "\\(.*\\)\\.c"
              then Regexp_.matched1 base ^ (spf ".o%c" thechar)
              else base ^ (spf ".o%c" thechar)
            in
            outfile := res;
            res
          end
          else outstr
          ) |> Fpath.v
          (*e: [[CLI.main()]] main code path, define [[outfile]] *)
        in
        
        (*s: [[CLI.main()]] main code path, define macropreprocessor [[conf]] *)
        let system_paths : Fpath.t list =
          (*s: [[CLI.main()]] main code path, define [[system_paths]] *)
            (try CapSys.getenv caps "INCLUDE" |> Str.split (Str.regexp "[ \t]+")
            with Not_found ->
              [spf "/%s/include" thestring; 
               "/sys/include";
              ] |> (fun xs -> if !ape then "/sys/include/ape"::xs else xs)
            ) |> Fpath_.of_strings
          (*e: [[CLI.main()]] main code path, define [[system_paths]] *)
        in
        let conf = Preprocessor.{
          defs = !macro_defs;
          (* this order? *)
          paths = system_paths @ List.rev !include_paths;
          dir_source_file = Fpath.v (Filename.dirname cfile);
        }
        in
        (*e: [[CLI.main()]] main code path, define macropreprocessor [[conf]] *)
        outfile |> FS.with_open_out caps (fun chan ->
          compile caps conf arch (Fpath.v cfile) chan
        );
        (*e: [[CLI.main()]] main code path with [[cfile]] and [[outfile]] *)
        Exit.OK
    | _ -> 
      (* stricter: *)
        failwith 
          "compiling multiple files at the same time is not supported; use mk"
    )
    (*e: [[CLI.main()]] matching [[args]] and [[outfile]] *)
  with exn ->
    if Sys.file_exists !outfile
    then begin 
       Logs.info (fun m -> m "removing %s because of error" !outfile);
       FS.remove caps (Fpath.v !outfile);
    end;
    (*s: [[CLI.main()]] when [[exn]] *)
    (*s: [[CLI.main()]] when [[exn]] if [[backtrace]] *)
    if !backtrace
    then raise exn
    (*e: [[CLI.main()]] when [[exn]] if [[backtrace]] *)
    else 
      (*s: [[CLI.main()]] match [[exn]] *)
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from 5c? *)
          Error.errorexit (spf "%cc: %s" thechar s)

      (*s: [[CLI.main()]] match [[exn]] other cases *)
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          Error.errorexit (spf "%s:%d %s" !!file line s)
      (* ocaml-light: | Check.Error err | Typecheck.Error err | ...  *)
      | Check.Error err -> Error.errorexit (Check.string_of_error err)
      | Typecheck.Error err -> Error.errorexit (Check.string_of_error err)
      | Rewrite.Error err -> Error.errorexit (Check.string_of_error err)
      | Eval_const.Error err -> Error.errorexit (Check.string_of_error err)
      | Codegen.Error err -> Error.errorexit (Check.string_of_error err)
      (*e: [[CLI.main()]] match [[exn]] other cases *)

      | _ -> raise exn
      )
      (*e: [[CLI.main()]] match [[exn]] *)
    (*e: [[CLI.main()]] when [[exn]] *)
(*e: function [[CLI.main]] *)
(*e: CLI.ml *)
