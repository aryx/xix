(*s: version_control/zlib.mli *)
(*s: copyright camlzip *)
(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License, with     *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright camlzip *)
open Common

(*s: exception [[Zlib.Error]] *)
exception Error of string * string
(*e: exception [[Zlib.Error]] *)

(*s: signature [[Zlib.compress]] *)
val compress:
  ?level: int -> ?header: bool -> 
  (bytes -> int) -> (bytes -> int -> unit) -> unit
(*e: signature [[Zlib.compress]] *)

(*s: signature [[Zlib.compress_direct]] *)
val compress_direct:
  ?level: int -> ?header: bool -> (bytes -> int -> unit) ->
  (bytes -> int -> int -> unit) * (unit -> unit)
(*e: signature [[Zlib.compress_direct]] *)

(*s: signature [[Zlib.uncompress]] *)
val uncompress:
  ?header: bool -> (bytes -> int) -> (bytes -> int -> unit) -> unit
(*e: signature [[Zlib.uncompress]] *)

type stream

(*s: type [[Zlib.flush_command]] *)
type flush_command =
    Z_NO_FLUSH
  | Z_SYNC_FLUSH
  | Z_FULL_FLUSH
  | Z_FINISH
(*e: type [[Zlib.flush_command]] *)

external deflate_init: int -> bool -> stream = "camlzip_deflateInit"
external deflate:
  stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_deflate_bytecode" "camlzip_deflate"
external deflate_end: stream -> unit = "camlzip_deflateEnd"

external inflate_init: bool -> stream = "camlzip_inflateInit"
external inflate:
  stream -> bytes -> int -> int -> bytes -> int -> int -> flush_command
         -> bool * int * int
  = "camlzip_inflate_bytecode" "camlzip_inflate"
external inflate_end: stream -> unit = "camlzip_inflateEnd"

external update_crc: int32 -> bytes -> int -> int -> int32
                   = "camlzip_update_crc32"
external update_crc_string: int32 -> string -> int -> int -> int32
                   = "camlzip_update_crc32"
(*e: version_control/zlib.mli *)
