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

(* alt: 
 *  - Exit_with_status in OCaml codebase 
 *  - { code: int; detail: string} as in Semgrep codebase (Exit_code.ml)
 *    and then specific abstract exit constants (e.g., Exit_code.fatal_error)
 *)
type t =
  (* code 0 in Unix *)
  | OK
  (* code 1 in Unix. Note that This is similar to Plan0's exits() *)
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
(* API *)
(*****************************************************************************)

let exit _caps t =
  let code = to_code t in
  (* nosemgrep: do-not-use-exit *)
  exit code

(* similar to Printexc.catch *)
let catch caps (f : unit -> t) : unit =
  let x = 
    try
      f ()
    with
    (* other exceptions (e.g., Failure) will still bubble up *)
    | ExitCode n -> Code n
    | Error s -> Err s
  in
  exit caps x
