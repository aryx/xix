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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Most of the code below comes from src: https://github.com/mirage/ocaml-hex
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type Hexsha.t *)
(* a 40 characters string, e.g. "d670460b4b4aece5915caf5c68d12f560a9fe3e4" *)
type t = string
(*e: type Hexsha.t *)

(*s: function Hexsha.is_hexsha *)
let is_hexsha x =
 String.length x = 40 && x =~ "^[0-9a-fA-F]+$"
(*e: function Hexsha.is_hexsha *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* start of copy-pasted code from ocaml-hex *)

let hexa = "0123456789abcdef"
and hexa1 =
  "0000000000000000111111111111111122222222222222223333333333333333\
   4444444444444444555555555555555566666666666666667777777777777777\
   88888888888888889999999999999999aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb\
   ccccccccccccccccddddddddddddddddeeeeeeeeeeeeeeeeffffffffffffffff"
and hexa2 =
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\
   0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

(*s: function Hexsha.of_string_fast *)
let of_string_fast s =
  let len = String.length s in
  let buf = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    Bytes.unsafe_set buf (i * 2)
      (String.unsafe_get hexa1 (Char.code (String.unsafe_get s i)));
    Bytes.unsafe_set buf (succ (i * 2))
      (String.unsafe_get hexa2 (Char.code (String.unsafe_get s i)));
  done;
  (*pad:`Hex*) buf
(*e: function Hexsha.of_string_fast *)

(*s: function Hexsha.to_char *)
let to_char x y =
  let code c = 
    match c with
    | '0'..'9' -> Char.code c - 48 (* Char.code '0' *)
    | 'A'..'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
    | 'a'..'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
    | _ -> 
      raise (Invalid_argument 
              (spf "Hex.to_char: %d is an invalid char" (Char.code c)))
  in
  Char.chr (code x lsl 4 + code y)
(*e: function Hexsha.to_char *)

(*s: function Hexsha.to_string *)
let to_string ((*`Hex*) s) =
  if s = "" 
  then ""
  else
    let n = String.length s in
    let buf = Bytes.create (n/2) in
    let rec aux i j =
      if i >= n 
      then ()
      else if j >= n 
           then raise (Invalid_argument "hex conversion: invalid hex string")
      else (
        Bytes.set buf (i/2) (to_char s.[i] s.[j]);
        aux (j+1) (j+2)
      )
    in
    aux 0 1;
    buf
(*e: function Hexsha.to_string *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function Hexsha.of_sha *)
let of_sha x =
  assert (Sha1.is_sha x);
  of_string_fast x
(*e: function Hexsha.of_sha *)

(*s: function Hexsha.to_sha *)
let to_sha x =
  assert (is_hexsha x);
  to_string x
(*e: function Hexsha.to_sha *)

(*s: function Hexsha.read *)
let read ch =
  let s = IO.really_nread ch 40 in
  assert (is_hexsha s);
  s
(*e: function Hexsha.read *)
(*s: function Hexsha.write *)
let write ch x =
  IO.nwrite ch x
(*e: function Hexsha.write *)
(*e: version_control/hexsha.ml *)
