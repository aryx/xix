(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

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
