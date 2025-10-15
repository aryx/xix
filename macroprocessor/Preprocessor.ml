(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Data structure containing the command-line data that one can pass to cpp
 * (e.g, -D and -I CLI flags).
 * This is passed then to Parse_cpp.parse() in addition to hooks.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type conf = {
  (* -D *)
  defs: (string * string) list;
  (* -I + system paths (e.g., /usr/include) *)
  paths: Fpath.t list;
  (* the directory of the C file so it is looked for "" but not for <> *)
  dir_source_file: Fpath.t;
}
