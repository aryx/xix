(* Martin Jambon, Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
(* Extensions to Fpath.ml
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let of_strings strings = List.map Fpath.v strings
let to_strings paths = List.map Fpath.to_string paths

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

module Operators = struct
  let ( / ) = Fpath.( / )
  let ( // ) = Fpath.( // )
  let ( !! ) = Fpath.to_string
end
