(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type entry = {
  path: string;
  mode: Index.mode;
  content: Sha1.t;
}

(* entry below refers only to files (not dirs), and their name
 * are adjusted to show a relative path from the root of the
 * project.
 *)
type t = 
  | Add of entry
  | Del of entry
  | Modify of entry * entry
  (* less: Rename, Copy *)
  (*| Identical of Tree.entry *)
