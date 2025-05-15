(* Martin Jambon, Yoann Padioleau
 *
 * Copyright (C) 2024-2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small capability-aware wrapper around Stdlib.exit.
 *
 * See also Exception.ml
 *
 * TODO:
 *  - move in lib_system/unix/ or lib_system/posix/ at some point
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type code = int

type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 or more in Unix. Note that This is similar to Plan's exits() *)
  | Err of string
  | Code of int

exception Error of string
exception ExitCode of int

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let to_code (x : t) : code =
  match x with
  | OK -> 0
  | Err str ->
     Logs.err (fun m -> m "%s" str);
     1
  | Code n -> n

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let exit _caps t =
  let code = to_code t in
  (* nosemgrep: do-not-use-exit *)
  exit code
