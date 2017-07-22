(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 * 
 * Most of the code below derives from: https://github.com/mirage/ocaml-git
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type sign = Plus | Minus

type tz_offset = {
  sign: sign;
  hours: int;
  min: int;
}

type t = {
  name : string;
  email: string;
  date : int64 * tz_offset(*option*);
}

(* less: default_tz_offset ? *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
let sign_of_char = function
  | '+' -> Plus
  | '-' -> Minus
  | c -> failwith (spf "User.sign_of_string: not a sign, got %c" c)

let read ch =
  let name = IO_utils.read_string_and_stop_char ch '<' in
  let email = IO_utils.read_string_and_stop_char ch '>' in
  let c = IO.read ch in
  if c <> ' ' then failwith "User.read: wrong format, missing space";

  let seconds = IO_utils.read_string_and_stop_char ch ' ' in
  let sign = IO.read ch in
  let hours = IO.nread_string ch 2 in
  let mins = IO.nread_string ch 2 in
  { name = String.sub name 0 (String.length name - 1);
    email = email;
    date = (Int64.of_string seconds,
            {
              sign = sign_of_char sign;
              hours = int_of_string hours;
              min = int_of_string mins;
            });
  }

let write_date ch (date, tz) =
  IO.nwrite ch (Int64.to_string date);
  IO.write ch ' ';
  let sign = match tz.sign with Plus -> "+" | Minus -> "-" in
  IO.nwrite ch (spf "%s%02d%02d" sign tz.hours tz.min)

let write ch user =
  IO.nwrite ch (spf "%s <%s> " user.name user.email);
  write_date ch user.date

