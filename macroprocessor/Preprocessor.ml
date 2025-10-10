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
type system_paths = Fpath.t list

(* The first element in the list is supposed to contain the directory
 * of the C file so it is looked for "" but not for <>
 *)
type include_paths = Fpath.t * system_paths

(* TODO: type conf = cmdline_defs * system_paths ? or include_paths?
 * or better record!
 *)
