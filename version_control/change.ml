(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type content = bytes

type entry = {
  (* relative path *)
  path: Common.filename;
  mode: Index.mode;
  content: content Lazy.t;
}

(* entry below refers only to files (not dirs), and their name
 * are adjusted to show a relative path from the root of the
 * project.
 *)
type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry (* before / after *)
  (* less: Rename, Copy *)
  (*| Identical of Tree.entry *)
