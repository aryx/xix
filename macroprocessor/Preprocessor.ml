(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* -D *)
type cmdline_defs = (string * string) list

(* -I *)
type system_paths = Common.filename list

(* The first element in the list is supposed to contain the directory
 * of the C file so it is looked for "" but not for <>
 *)
type include_paths = Common.filename * system_paths
