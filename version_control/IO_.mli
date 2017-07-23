
val read_string_and_stop_char: 
  IO.input -> char -> string

val read_int_and_nullbyte: 
  IO.input -> int

val read_key_space_value_newline: 
  IO.input -> string (* key *) -> (IO.input -> 'a) -> 'a

val with_close_out: ('a IO.output -> unit) -> 'a IO.output -> 'a
