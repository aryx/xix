(*s: version_control/IO_.mli *)

(*s: signature [[IO_.read_string_and_stop_char]] *)
val read_string_and_stop_char: 
  IO.input -> char -> string
(*e: signature [[IO_.read_string_and_stop_char]] *)

(*s: signature [[IO_.read_int_and_nullbyte]] *)
val read_int_and_nullbyte: 
  IO.input -> int
(*e: signature [[IO_.read_int_and_nullbyte]] *)

(*s: signature [[IO_.read_key_space_value_newline]] *)
val read_key_space_value_newline: 
  IO.input -> string (* key *) -> (IO.input -> 'a) -> 'a
(*e: signature [[IO_.read_key_space_value_newline]] *)

(*s: signature [[IO_.with_close_out]] *)
val with_close_out: ('a IO.output -> unit) -> 'a IO.output -> 'a
(*e: signature [[IO_.with_close_out]] *)
(*e: version_control/IO_.mli *)
