(*s: version_control/unzip.mli *)
(*
 * Unzip - inflate format decompression algorithm
 * Copyright (C) 2004 Nicolas Cannasse
 * Compliant with RFC 1950 and 1951
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Stdcompat (* for bytes *)
(** Decompression algorithm.

  Unzip decompression algorithm is compliant with RFC 1950 and 1951 which
  are describing the "inflate" algorithm used in most popular file formats.
  This format is also the one used by the popular ZLib library.  
*)

(*s: type [[Unzip.error_msg]] *)
type error_msg =
  | Invalid_huffman
  | Invalid_data
  | Invalid_crc
  | Truncated_data
  | Unsupported_dictionary
(*e: type [[Unzip.error_msg]] *)

(*s: exception [[Unzip.Error]] *)
exception Error of error_msg
(*e: exception [[Unzip.Error]] *)

(*s: signature [[Unzip.inflate]] *)
val inflate : ?header:bool -> IO.input -> IO.input
(** wrap an input using "inflate" decompression algorithm. raises [Error] if
  an error occurs (this can only be caused by malformed input data). *)
(*e: signature [[Unzip.inflate]] *)



(* internals *)
type t

(*s: signature [[Unzip.inflate_init]] *)
val inflate_init : ?header:bool -> IO.input -> t
(*e: signature [[Unzip.inflate_init]] *)
(*s: signature [[Unzip.inflate_data]] *)
val inflate_data : t -> bytes -> int -> int -> int
(*e: signature [[Unzip.inflate_data]] *)

val debug: bool ref

(*e: version_control/unzip.mli *)
