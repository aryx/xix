(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let with_close_out f ch =
  f ch;
  let res = IO.close_out ch in
  res

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

let read_int_and_nullbyte ch =
  let str = IO.read_c_string ch in
  int_of_string str

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
