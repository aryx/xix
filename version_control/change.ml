(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* entry below refers only to files (not dirs), and their name
 * are adjusted to show a relative path from the root of the
 * project.
 *)
type t = 
  | Add of Tree.entry
  | Del of Tree.entry
  | Modify of Tree.entry * Tree.entry
  (* less: Rename, Copy *)
  (*| Identical of Tree.entry *)
