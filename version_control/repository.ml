(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 *
 * less: use nested modules for objects, index, refs below?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type t = {
  root: Common.filename;
  dotgit: Common.filename;

  (* less: compression level? *)
}

let (/) = Filename.concat

(*****************************************************************************)
(* Repo create/open/close *)
(*****************************************************************************)

let init root =
  raise Todo

let open_ root = 
  raise Todo
let close t =
  raise Todo

let with_repo root =
  raise Todo

let clone r dst =
  raise Todo

(*****************************************************************************)
(* Objects *)
(*****************************************************************************)
let read_obj r h =
  raise Todo

let mem_obj r h =
  raise Todo

let write_obj r h obj =
  raise Todo

(*****************************************************************************)
(* Refs *)
(*****************************************************************************)
let read_ref r aref =
  raise Todo

let remove_ref r aref =
  raise Todo

let add_ref r aref refval =
  raise Todo

let test_and_set_ref r aref refval =
  raise Todo

(*****************************************************************************)
(* Index *)
(*****************************************************************************)
let read_index r =
  raise Todo

let write_index r idx =
  raise Todo


(*****************************************************************************)
(* Packs *)
(*****************************************************************************)
