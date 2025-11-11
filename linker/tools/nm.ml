(*s: tools/nm.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Ast_asm
module A = Ast_asm

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of nm, the Plan 9 object/library/executable symbol inspector.
 * 'nm' probably stands for "names" or "name map".
 *
 * Main limitations compared to nm:
 *  - 
 *
 * todo:
 *  - handle executable too
 *
 * later:
 *)

(*****************************************************************************)
(* Types, constants, and globals *)
(*****************************************************************************)
(*s: type [[Nm.caps]] *)
(* Needs:
 *  - open_in: should be only for argv-derived files
 *  - stdout: the T|D|U ... output of nm
 *)
type caps = < Cap.open_in; Cap.stdout >
(*e: type [[Nm.caps]] *)

(*s: constant [[Nm.usage]] *)
let usage = 
  "usage: nm [-options] file ..."
(*e: constant [[Nm.usage]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Nm.visit_obj]] *)
let visit_obj (caps : < Cap.stdout; .. >) (obj : 'instr Object_file.t) : unit =
  let (xs, _locs) = obj.prog in
  (* step1: visit the defs *)
  let defs = Hashtbl_.create () in
  xs |> List.iter (fun (line, _loc) ->
      match line with
      | Pseudo (TEXT (glob, _attrs, _int)) ->
          Hashtbl.add defs glob true;
          let ident = A.s_of_global glob in
          Console.print caps (spf " T %s" ident)
      | Pseudo (GLOBL (glob, _attrs, _int)) ->
          Hashtbl.add defs glob true;
          let ident = A.s_of_global glob in
          Console.print caps (spf " D %s" ident)
      | Pseudo (DATA _ | WORD _) -> ()
      | Virtual (RET | NOP) -> ()
      (* will visit the uses below *)
      | Instr _x -> ()
      | LabelDef _ -> raise (Impossible "objects should not have LabelDef")
  );
  (* step2: visit the uses *)
  let arch_methods : 'instr Arch_linker.t = Arch_linker.of_arch obj.arch in
  xs |> List.iter (fun (line, _loc) ->
      match line with
      | Instr x ->
          x |> arch_methods.visit_globals_instr (fun glob ->
              if not (Hashtbl.mem defs glob)
              then
                let ident = A.s_of_global glob in
                Console.print caps (spf " U %s" ident)
          )
      | Pseudo _ | Virtual _ | LabelDef _ -> ()
   )

(*e: function [[Nm.visit_obj]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

(*s: function [[Nm.nm]] *)
let nm (caps : < caps; ..> ) (file : Fpath.t) : unit =
  match () with
  | _ when Object_file.is_obj_filename file ->
      let obj : 'instr Object_file.t = 
        FS.with_open_in caps Object_file.load file
      in
      visit_obj caps obj
  | _ when Library_file.is_lib_filename file ->
      let objs : 'instr Library_file.t =
        FS.with_open_in caps Library_file.load file
      in
      objs |> List.iter (visit_obj caps)
  | _ -> 
      failwith "TODO: handle exec format"
(*e: function [[Nm.nm]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Nm.main]] *)
let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  let infiles = ref [] in

  let backtrace = ref false in
  let level = ref (Some Logs.Warning) in

  let options = ([
   (* TODO: support the many nm flags *)
  ] @ Logs_.cli_flags level
  ) |> Arg.align
  in
  (try
    Arg.parse_argv argv options
      (fun f -> infiles := Fpath.v f::!infiles) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "nm ran from %s" (Sys.getcwd()));

  (match List.rev !infiles with
  | [] -> 
      Arg.usage options usage; 
      Exit.Code 1
  | xs -> 
     try 
        (* the main call *)
        xs |> List.iter (nm caps);
        Exit.OK
     with exn ->
     if !backtrace
     then raise exn
     else 
      (match exn with
      | Failure s ->
          Logs.err (fun m -> m "%s" s);
          Exit.Code 1
      | _ -> raise exn
      )
  )
(*e: function [[Nm.main]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: constant [[Nm._]] *)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     Exit.exit caps (Exit.catch (fun () -> main caps (CapSys.argv caps)))
  )
(*e: constant [[Nm._]] *)
(*e: tools/nm.ml *)
