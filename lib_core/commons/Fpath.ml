(* Copyright 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Poor's man filename library following the interface defined in
 * https://github.com/dbuenzli/fpath by Daniel Bunzli.
 *
 * Similar to Logs we define our own Fpath to limit external dependencies
 * in XiX.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = string (* N.B. a path is never "" or something is wrooong. *)
[@@deriving show]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let v (s : string) : t =
  if s = ""
  then failwith "invalid empty path"
  else s

let to_string (p : t) : string = p

let add_seg (p : t) (seg : string) : t =
  Filename.concat p seg

let append (p1 : t) (p2 : t) : t =
  Filename.concat p1 p2
