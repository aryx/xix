(* Yoann Padioleau, Martin Jambon
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
 *  - LATER move in lib_system/unix/ or lib_system/posix/ at some point
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type code = int
[@@deriving show]

let show _ = "NO DERIVING"
[@@warning "-32"]

(* alt: 
 *  - Exit_with_status in OCaml codebase 
 *  - { code: int; detail: string} as in Semgrep codebase (Exit_code.ml)
 *    and then specific abstract exit constants (e.g., Exit_code.fatal_error)
 * history: 
 *  - Common.UnixExit
 *)
type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 in Unix. Note that This is similar to Plan0's exits() *)
  | Err of string
  (* This must be > 0 otherwise use OK *)
  | Code of code
[@@deriving show]

exception ExitCode of code
(* alt: could also add 
 *   exception Error of string
 *
 * but not used for now and we usually instead just use Failure
 * to encode similar information.
 *)

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
(* API *)
(*****************************************************************************)

let exit caps t =
  let _ = caps#exit in
  let code = to_code t in
  (* nosemgrep: do-not-use-exit *)
  exit code

(* a bit similar to Printexc.catch *)
let catch (f : unit -> t) : t =
  try
    f ()
  with
  (* other exceptions (e.g., Failure) will still bubble up *)
  | ExitCode 0 -> OK
  | ExitCode n -> Code n
