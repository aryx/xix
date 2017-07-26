(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* entry below refers only to files (not dirs) *)
type t = 
  | Add of Tree.entry
  | Del of Tree.entry
  | Modify of Tree.entry * Tree.entry
  (* less: Rename, Copy *)
  (*| Identical of Tree.entry *)
