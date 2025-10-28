(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Ast_asm
module A = Ast_asm

(* for ocaml-light field access *)
open Object_file

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of nm, the Plan 9 object/executable/library symbol inspector.
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
type caps = < Cap.open_in; Cap.stdout >

let usage = 
  "usage: nm [-options] file ..."

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: visit also the uses, not just the defs *)
let visit_obj (caps : < Cap.stdout; .. >) (obj : 'instr Object_file.t) : unit =
  let (xs, _locs) = obj.prog in
  (* visit the defs *)
  xs |> List.iter (fun (line, _loc) ->
      match line with
      | Pseudo (TEXT (glob, _attrs, _int)) ->
          let ident = A.s_of_global glob in
          Console.print caps (spf " T %s" ident)
      | Pseudo (GLOBL (glob, _attrs, _int)) ->
          let ident = A.s_of_global glob in
          Console.print caps (spf " D %s" ident)
      | Pseudo (DATA _ | WORD _) -> ()
      | Virtual (RET | NOP) -> ()
      (* TODO: visit the uses *)
      | Instr _x -> ()
      | LabelDef _ -> raise (Impossible "objects should not have LabelDef")
  )


(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : <caps; ..>) (argv : string array) : Exit.t =
  let infiles = ref [] in

  let backtrace = ref false in
  let level = ref (Some Logs.Warning) in

  let options = [
   (* TODO: support the many nm flags *)
  ] |> Arg.align
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     Exit.exit caps (Exit.catch (fun () -> main caps (CapSys.argv caps)))
  )
