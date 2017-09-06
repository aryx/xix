(*s: version_control/change.ml *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*s: type Change.content *)
(*****************************************************************************)
type content = bytes
(*e: type Change.content *)

(*s: type Change.entry *)
type entry = {
  (* relative path *)
  path: Common.filename;
  mode: Index.mode;
  content: content Lazy.t;
}
(*e: type Change.entry *)

(*s: type Change.t *)
(* entry below refers only to files (not dirs), and their name
 * are adjusted to show a relative path from the root of the
 * project.
 *)
type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry (* before / after *)
(*e: type Change.t *)
  (* less: Rename, Copy *)
  (*| Identical of Tree.entry *)
(*e: version_control/change.ml *)
