(*s: version_control/hexsha.ml *)
(*s: copyright ocaml-hex *)
(*
 * Copyright (c) 2015 Trevor Summers Smith <trevorsummerssmith@gmail.com>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
(*e: copyright ocaml-hex *)
open Common
open Regexp_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Most of the code below comes from src: https://github.com/mirage/ocaml-hex
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Hexsha.t]] *)
(* a 40 characters string, e.g. "d670460b4b4aece5915caf5c68d12f560a9fe3e4" *)
type t = string
(*e: type [[Hexsha.t]] *)

(*s: function [[Hexsha.is_hexsha]] *)
let is_hexsha x =
 String.length x = 40 && x =~ "^[0-9a-fA-F]+$"
(*e: function [[Hexsha.is_hexsha]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* start of copy-pasted code from ocaml-hex *)

let _hexa = "0123456789abcdef"
(*s: constant [[Hexsha.hexa1]] *)
and hexa1 =
  "0000000000000000111111111111111122222222222222223333333333333333\
   4444444444444444555555555555555566666666666666667777777777777777\
   88888888888888889999999999999999aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb\
   ccccccccccccccccddddddddddddddddeeeeeeeeeeeeeeeeffffffffffffffff"
(*e: constant [[Hexsha.hexa1]] *)
(*s: constant [[Hexsha.hexa2]] *)
and hexa2 =
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
(*e: constant [[Hexsha.hexa2]] *)

(*s: function [[Hexsha.to_char]] *)
let to_byte hex1 hex2 =
  let code hex = 
    match hex with
    | '0'..'9' -> Char.code hex - Char.code '0'
    | 'A'..'F' -> Char.code hex - (Char.code 'A' + 10)
    | 'a'..'f' -> Char.code hex - (Char.code 'a' + 10)
    | _ -> 
      raise (Invalid_argument 
              (spf "Hex.to_byte: %d is an invalid hexadecimal digit" (Char.code hex)))
  in
  Char.chr (((code hex1) lsl 4) + (code hex2))
(*e: function [[Hexsha.to_char]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Hexsha.of_sha]] *)
let of_sha s =
  assert (Sha1.is_sha s);
  let n = String.length s in
  let buf = Bytes.create (n * 2) in
  (*s: [[Hexsha.of_sha()]] fill [[buf]] *)
  for i = 0 to n - 1 do
    Bytes.set buf (i * 2) hexa1.[Char.code (s.[i])];
    Bytes.set buf ((i * 2) + 1) hexa2.[Char.code (s.[i])];
  done;
  (*e: [[Hexsha.of_sha()]] fill [[buf]] *)
  Bytes.to_string buf
(*e: function [[Hexsha.of_sha]] *)

(*s: function [[Hexsha.to_sha]] *)
let to_sha s =
  assert (is_hexsha s);
  let n = String.length s in
  let buf = Bytes.create (n/2) in
  (*s: [[Hexsha.to_sha()]] fill [[buf]] *)
  let rec aux i =
    if i >= n 
    then ()
    else begin
      (*s: [[hexsha.to_sha()]] double sanity check range i *)
      if i+1 >= n 
      then raise (Invalid_argument "hex conversion: invalid hex string");
      (*e: [[hexsha.to_sha()]] double sanity check range i *)
      Bytes.set buf (i/2) (to_byte s.[i] s.[i+1]);
      aux (i+2)
    end
  in
  aux 0;
  (*e: [[Hexsha.to_sha()]] fill [[buf]] *)
  Bytes.to_string buf
(*e: function [[Hexsha.to_sha]] *)

(*s: function [[Hexsha.read]] *)
let read ch =
  let s = IO.really_nread_string ch 40 in
  assert (is_hexsha s);
  s
(*e: function [[Hexsha.read]] *)
(*s: function [[Hexsha.write]] *)
let write ch x =
  IO.nwrite_string ch x
(*e: function [[Hexsha.write]] *)
(*e: version_control/hexsha.ml *)
