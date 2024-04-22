(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of 5c, the Plan 9 C compiler for ARM.
 *
 * Main limitations compared to 5c:
 *  - no unicode support
 *  - can not compile multiple files at the same time
 *    (but you should use mk anyway)
 *  - can not compile from stdin
 *    (but who uses that?)
 *  - no -. to remove auto search for header in current directory
 *    (but who uses that?)
 *  - no error recovery, we stop at the first error (except in check.ml)
 *    (but compiler now fast enough and errors have a domino effect anyway)
 *  - stricter for grammar (see parser.mly), for instance force a specific
 *    order between the sign, qualifier, and type.
 *  - disallow implicit declarations of functions
 *  - stricter for typechecking (see typecheck.ml), for instance
 *    we do not support void* conversions (5c -V), and we use name
 *    equality for typechecking structs, not field equality.
 *    we also do not automatically transform 0 in nil; I force to write
 *    nil
 *  - no support for certain kencc extensions: 
 *     * STILL? unnamed structure element
 *       (confusing anyway)
 *     * typestr
 *       (seems dead)
 *  - no support for certain C features:
 *     * enum float
 *       (who uses that?)
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
 *  - safe-linking support
 *  - debugger support
 *  - profiler support
 *)

let thechar = '5'
let thestring = "arm"
let arch = Arch5.arch

let usage = 
  spf "usage: %cc [-options] files" thechar

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let do_action s xs =
  match s with
  | "-test_parser" ->
      xs |> List.iter (fun file ->
        pr2 (spf "processing %s" file);
        let system_paths = 
          [spf "/%s/include" thestring; "/sys/include";]
        in
        try 
          let _ = Parse.parse ([], (".", system_paths)) file in
          ()
        with Location_cpp.Error (s, loc) ->
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          failwith (spf "%s:%d %s" file line s)
      )

  | _ -> failwith ("action not supported: " ^ s)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
let compile (defs, include_paths) infile outfile =

  let ast = Parse.parse (defs, include_paths) infile in

  (* debug *)
  if !Flags.dump_ast
  then pr2 (Dumper.s_of_any (Ast.Program ast));

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
        pr2 (Ast.unwrap k);
        pr2 (Dumper.s_of_any (Ast.FinalType v.Typecheck.typ));
      | _ -> ()
    );
    funcs |> List.iter (fun func ->
      pr2 (Dumper.s_of_any_with_types (Ast.Toplevel (Ast.FuncDef func)))
    );
  end;

  (* todo: Rewrite.rewrite *)

  let asm = Codegen5.codegen (ids, structs, funcs) in

  if !Flags.dump_asm
  then begin
    let pc = ref 0 in
    asm |> List.iter (fun (instr, _loc) ->
      (* less: use a assembly pretty printer instead? easier to debug? 5c -S *)
      let v = Meta_ast_asm5.vof_line instr in
      pr2 (spf "%2d: %s" !pc (Ocaml.string_of_v v));
      incr pc;
    );
  end;

  Object_code5.save (asm, !Location_cpp.history) outfile



(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =

  (* in *)
  let args = ref [] in
  (* out *)
  let outfile = ref "" in

  (* for cpp *)
  let system_paths = ref [] in
  let defs = ref [] in
  (* Ansi Posix Environment for plan9 *)
  let ape = ref false in 

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
      defs := (var, val_)::!defs
    ), " <name=def> (or just <name>) define name for preprocessor";
    "-I", Arg.String (fun s ->
      system_paths := s::!system_paths
    ), " <dir> add dir as a path to look for '#include <file>' files";
    "-ape", Arg.Set ape,
    " ";

    "-w", Arg.Set Flags.warn,
    " enable warnings";
    "-werror", Arg.Set Flags.warnerror,
    " warnings generate error exceptions";

    "-S", Arg.Set Flags.dump_asm,
    " dump the generated assembly";

    "-e", Arg.Set Flags_cpp.debug_inclusion, " ";
    "-f", Arg.Set Flags_cpp.debug_line, " ";
    "-m", Arg.Set Flags_cpp.debug_macros, " ";
    (* pad: I added long names for those options *)
    "-debug_inclusion", Arg.Set Flags_cpp.debug_inclusion, " ";
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
    "-debugger", Arg.Set Flags.debugger,
    " ";
    "-backtrace", Arg.Set backtrace,
    " dump the backtrace after an error";
  ]
  in
  Arg.parse (Arg.align options) (fun t -> 
    args := t::!args
  ) usage;
  (* less: process the old style -Dname=val and -Idir attached *)

  (* to test and debug components of mk *)
  if !action <> "" then begin 
    do_action !action (List.rev !args); 
    exit 0 
  end;

  try 
    (match !args, !outfile with
    | [], "" -> 
        Arg.usage (Arg.align options) usage;
        Error.errorexit ""
    | [cfile], outfile ->
        let base = Filename.basename cfile in
        let dir = Filename.dirname cfile in
        let system_paths =
          (try Sys.getenv "INCLUDE" |> Str.split (Str.regexp "[ \t]+")
          with Not_found ->
            [spf "/%s/include" thestring; 
             "/sys/include";
            ] |> (fun xs -> if !ape then "/sys/include/ape"::xs else xs)
          ) @
          !system_paths
        in
        
        let outfile = 
          if outfile = ""
          then
            if base =~ "\\(.*\\)\\.c"
            then Regexp_.matched1 base ^ (spf ".%c" thechar)
            else base ^ (spf ".%c" thechar)
          else outfile
        in
        compile (!defs, (dir, system_paths)) cfile outfile
    | _ -> 
      (* stricter: *)
        failwith 
          "compiling multiple files at the same time is not supported; use mk"
    )
  with exn ->
    if !backtrace || !Flags.debugger
    then raise exn
    else 
      (match exn with
      | Failure s -> 
          (* useful to indicate that error comes from 5c? *)
          Error.errorexit (spf "%cc: %s" thechar s)
      | Location_cpp.Error (s, loc) ->
          (* less: could use final_loc_and_includers_of_loc loc *)
          let (file, line) = Location_cpp.final_loc_of_loc loc in
          Error.errorexit (spf "%s:%d %s" file line s)
      | Check.Error err | Typecheck.Error err | Eval_const.Error err
      | Codegen5.Error err 
        ->
          Error.errorexit (Check.string_of_error err)

      | _ -> raise exn
      )

let _ = 
    main ()
