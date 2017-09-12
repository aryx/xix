(*s: version_control/IO_.ml *)
(*s: copyright ocamlgit *)
(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
(*e: copyright ocamlgit *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function IO_.with_close_out *)
let with_close_out f ch =
  f ch;
  let res = IO.close_out ch in
  res
(*e: function IO_.with_close_out *)

(*s: function IO_.read_string_and_stop_char *)
let read_string_and_stop_char ch stop_char =
  let b = Buffer.create 8 in
  let rec loop() =
    let c = IO.read ch in
    if c <> stop_char then begin
      Buffer.add_char b c;
      loop();
    end;
  in
  loop();
  Buffer.contents b
(*e: function IO_.read_string_and_stop_char *)

(*s: function IO_.read_int_and_nullbyte *)
let read_int_and_nullbyte ch =
  let str = IO.read_c_string ch in
  int_of_string str
(*e: function IO_.read_int_and_nullbyte *)

(*s: function IO_.read_key_space_value_newline *)
let read_key_space_value_newline ch k f =
  let str = read_string_and_stop_char ch ' ' in
  if str <> k
  then failwith (spf 
    "read_key_space_value_newline: wrong key got %s (expecting %s)"
    str k);
  let v = f ch in
  let c = IO.read ch in
  if c <> '\n'
  then failwith "read_key_space_value_newline: wrong format, no newline";
  v
(*e: function IO_.read_key_space_value_newline *)
(*e: version_control/IO_.ml *)
