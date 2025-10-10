(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

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
