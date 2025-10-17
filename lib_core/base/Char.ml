(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Character operations *)

external code: char -> int = "%identity"
external unsafe_chr: int -> char = "%identity"

let chr n =
  if n < 0 or n > 255 then invalid_arg "Char.chr" else unsafe_chr n

(* old: external is_printable: char -> bool = "is_printable" *)

external string_create: int -> string = "caml_create_string"
external string_unsafe_get : string -> int -> char = "%string_unsafe_get"
external string_unsafe_set : string -> int -> char -> unit
                           = "%string_unsafe_set"

let escaped = function
    '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
(* old:
  | c ->  if is_printable c then begin
            let s = string_create 1 in
            string_unsafe_set s 0 c;
            s
          and else begin ... end
*)
  | ' ' .. '~' as c ->
            let s = string_create 1 in
            string_unsafe_set s 0 c;
            s
  | c -> 
          begin
            let n = code c in
            let s = string_create 4 in
            string_unsafe_set s 0 '\\';
            string_unsafe_set s 1 (unsafe_chr (48 + n / 100));
            string_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
            string_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
            s
          end

let lowercase c =
  if (c >= 'A' && c <= 'Z')
  || (c >= '\192' && c <= '\214')
  || (c >= '\216' && c <= '\222')
  then unsafe_chr(code c + 32)
  else c

let uppercase c =
  if (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')
  then unsafe_chr(code c - 32)
  else c


type t = char

let compare c1 c2 = code c1 - code c2
let equal (c1: t) (c2: t) = compare c1 c2 = 0
