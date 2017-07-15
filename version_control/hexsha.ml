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

type t = string

let is_hexsha x =
 String.length x = 40
 (* less: could also check every chars is between 0 and F,
  * or just call to_sha and catch if any exn.
  *)

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


let invalid_arg fmt =
  Printf.ksprintf (fun str -> raise (Invalid_argument str)) fmt

let to_char x y =
  let code c = match c with
    | '0'..'9' -> Char.code c - 48 (* Char.code '0' *)
    | 'A'..'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
    | 'a'..'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
    | _ -> invalid_arg "Hex.to_char: %d is an invalid char" (Char.code c)
  in
  Char.chr (code x lsl 4 + code y)

let to_helper ~empty_return ~create ~set ((*`Hex*) s) =
  if s = "" then empty_return
  else
    let n = String.length s in
    let buf = create (n/2) in
    let rec aux i j =
      if i >= n then ()
      else if j >= n then invalid_arg "hex conversion: invalid hex string"
      else (
        set buf (i/2) (to_char s.[i] s.[j]);
        aux (j+1) (j+2)
      )
    in
    aux 0 1;
    buf

let to_string hex =
  to_helper ~empty_return:"" ~create:Bytes.create ~set:Bytes.set hex


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let of_sha x =
  assert (Sha.is_sha x);
  of_string_fast x

let to_sha x =
  to_string x
