(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp_.Operators
open Fpath_.Operators

(* for field-access in ocaml-light *)
open Chan

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple API to load/save archive library files (e.g., libc.a) *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* An archive (.a) is really essentially just a list of objects, which in Plan 9
 * are just a list of serialized assembly ASTs
 *
 * TODO: do SYMDEF/ranlib indexing so can avoid objects that are not
 * needed by the linked program like in 5l/vl/...
 *)
type 'instr t = 'instr Ast_asm.program list

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let save (x : 'instr t) (chan : Chan.o) : unit =
  Logs.info (fun m -> m "Saving library in %s" (Chan.destination chan));
  output_value chan.oc (Object_file.version, x)

let load (chan : Chan.i) : 'instr t =
  Logs.info (fun m -> m "Loading library %s" (Chan.origin chan));
  let (ver, x) = input_value chan.ic in
  if ver <> Object_file.version
  then raise Object_file.WrongVersion
  else x

let is_lib_filename (file : Fpath.t) : bool =
  !!file =~ ".*\\.oa[5vi]?$"
