(*s: version_control/user.ml *)
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
(*s: type User.sign *)
(*****************************************************************************)

type sign = Plus | Minus
(*e: type User.sign *)

(*s: type User.tz_offset *)
type tz_offset = {
  sign: sign;
  hours: int;
  min: int;
}
(*e: type User.tz_offset *)

(*s: type User.t *)
type t = {
  name : string;
  email: string;
  date : int64 * tz_offset(*option*);
}
(*e: type User.t *)

(* less: default_tz_offset ? *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*s: function User.sign_of_char *)
let sign_of_char = function
  | '+' -> Plus
  | '-' -> Minus
  | c -> failwith (spf "User.sign_of_string: not a sign, got %c" c)
(*e: function User.sign_of_char *)

(*s: function User.char_of_sign *)
let char_of_sign = function
  | Plus -> '+'
  | Minus -> '-'
(*e: function User.char_of_sign *)

(*s: function User.read *)
let read ch =
  let name = IO_.read_string_and_stop_char ch '<' in
  let email = IO_.read_string_and_stop_char ch '>' in
  let c = IO.read ch in
  if c <> ' ' then failwith "User.read: wrong format, missing space";

  let seconds = IO_.read_string_and_stop_char ch ' ' in
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
(*e: function User.read *)

(*s: function User.write_date *)
let write_date ch (date, tz) =
  IO.nwrite ch (Int64.to_string date);
  IO.write ch ' ';
  let sign = match tz.sign with Plus -> "+" | Minus -> "-" in
  IO.nwrite ch (spf "%s%02d%02d" sign tz.hours tz.min)
(*e: function User.write_date *)

(*s: function User.write *)
let write ch user =
  IO.nwrite ch (spf "%s <%s> " user.name user.email);
  write_date ch user.date
(*e: function User.write *)

(*****************************************************************************)
(* Show *)
(*****************************************************************************)

(*s: function User.string_of_date *)
let string_of_date (date, tz) =
  let f = Int64.to_float date in
  let tm = Unix.localtime f in

  spf "%s %s %d %02d:%02d:%02d %d %c%02d%02d"
    (Date.string_of_day tm.Unix.tm_wday) (Date.string_of_month tm.Unix.tm_mon) 
    tm.Unix.tm_mday 
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (tm.Unix.tm_year + 1900)
    (char_of_sign tz.sign) tz.hours tz.min
(*e: function User.string_of_date *)

(*e: version_control/user.ml *)
