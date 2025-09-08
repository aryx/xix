(*
 * IO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** High-order abstract I/O.

  IO module simply deals with abstract inputs/outputs. It provides a
  set of methods for working with these IO as well as several
  constructors that enable to write to an underlying channel, buffer,
  or enum.
*)

type input
(** The abstract input type. *)

type 'a output
(** The abstract output type, ['a] is the accumulator data, it is returned
  when the [close_out] function is called. *)

exception No_more_input
(** This exception is raised when reading on an input with the [read] or
  [nread] functions while there is no available token to read. *)

exception Input_closed
(** This exception is raised when reading on a closed input. *)

exception Output_closed
(** This exception is raised when reading on a closed output. *)


(** {6 Standard API} *)

val read : input -> char
(** Read a single char from an input or raise [No_more_input] if
  no input available. *)

val nread : input -> int -> bytes
(** [nread i n] reads a byte sequence of size up to [n] from an input.
  The function will raise [No_more_input] if no input is available.
  It will raise [Invalid_argument] if [n] < 0. *)

val nread_string : input -> int -> string
(** as [nread], but reads a string. *)

val really_nread : input -> int -> bytes
(** [really_nread i n] reads a byte sequence of exactly [n] characters
  from the input. Raises [No_more_input] if at least [n] characters are
  not available. Raises [Invalid_argument] if [n] < 0. *)

val really_nread_string : input -> int -> string
(** as [really_nread], but reads a string. *)

val input : input -> bytes -> int -> int -> int
(** [input i b p l] reads up to [l] characters from the given input, storing
  them in buffer [b], starting at character number [p]. It returns the actual
  number of characters read or raise [No_more_input] if no character can be
  read. It will raise [Invalid_argument] if [p] and [l] do not designate a
  valid subsequence of [b]. *)

val really_input : input -> bytes -> int -> int -> int
(** [really_input i b p l] reads exactly [l] characters from the given input,
  storing them in the buffer [b], starting at position [p]. For consistency with
  {!IO.input} it returns [l]. Raises [No_more_input] if at [l] characters are
  not available. Raises [Invalid_argument] if [p] and [l] do not designate a
  valid subsequence of [b]. *)

val close_in : input -> unit
(** Close the input. It can no longer be read from. *)



val write : 'a output -> char -> unit
(** Write a single char to an output. *)

val nwrite : 'a output -> bytes -> unit
(** Write a byte sequence to an output. *)

val nwrite_string : 'a output -> string -> unit
(** Write a string to an output. *)

val output : 'a output -> bytes -> int -> int -> int
(** [output o b p l] writes up to [l] characters from byte sequence [b], starting at
  offset [p]. It returns the number of characters written. It will raise
  [Invalid_argument] if [p] and [l] do not designate a valid subsequence of [b]. *)

val really_output : 'a output -> bytes -> int -> int -> int
(** [really_output o b p l] writes exactly [l] characters from byte sequence [b] onto
  the the output, starting with the character at offset [p]. For consistency with
  {!IO.output} it returns [l]. Raises [Invalid_argument] if [p] and [l] do not
  designate a valid subsequence of [b]. *)

val flush : 'a output -> unit
(** Flush an output. *)

val close_out : 'a output -> 'a
(** Close the output and return its accumulator data.
  It can no longer be written. *)



(** {6 Creation of IO Inputs/Outputs} *)

val input_string : string -> input
(** Create an input that will read from a string. *)

val input_bytes : bytes -> input
(** Create an input that will read from a byte sequence. *)


val output_string : unit -> string output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it. *)

val output_bytes : unit -> bytes output
(** Create an output that will write into a byte sequence in an efficient way.
  When closed, the output returns all the data written into it. *)


val output_strings : unit -> string list output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it.
  Several strings are used in case the output size excess max_string_length
*)


val input_channel : in_channel -> input
(** Create an input that will read from a channel. *)

val output_channel : out_channel -> unit output
(** Create an output that will write into a channel. *)


val create_in :
  (*read:*)(unit -> char) ->
  (*input:*)(Bytes.t -> int -> int -> int) -> (*close:*)(unit -> unit) -> input
(** Fully create an input by giving all the needed functions. *)

(*
val create_out :
  write:(char -> unit) ->
  output:(Bytes.t -> int -> int -> int) ->
  flush:(unit -> unit) -> close:(unit -> 'a) -> 'a output
(** Fully create an output by giving all the needed functions. *)
 *)

(** {6 Utilities} *)

(* val scanf : input -> ('a, 'b, 'c, 'd) Scanf.scanner *)
(** The scanf function works for any input. *)

(*val printf : 'a output -> ('b, unit, string, unit) format4 -> 'b *)
(** The printf function works for any output. *)

val read_all : input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val pipe : unit -> input * unit output
(** Create a pipe between an input and an ouput. Data written from
  the output can be read from the input. *)

val pos_in : input -> input * (unit -> int)
(** Create an input that provide a count function of the number of Bytes.t
  read from it. *)

val pos_out : 'a output -> 'a output * (unit -> int)
(** Create an output that provide a count function of the number of Bytes.t
  written through it. *)

external cast_output : 'a output -> unit output = "%identity"
(** You can safely transform any output to an unit output in a safe way
  by using this function. *)


(** {6 Binary files API}

  Here is some API useful for working with binary files, in particular
  binary files generated by C applications. By default, encoding of
  multibyte integers is low-endian. The BigEndian module provide multibyte
  operations with other encoding.
*)

val read_byte : input -> int
(** Read an unsigned 8-bit integer. *)

val read_signed_byte : input -> int
(** Read an signed 8-bit integer. *)

val read_c_string : input -> string
(** Read a null-terminated string. *)

val read_c_bytes : input -> bytes
(** Read a null-terminated byte sequence. *)

val read_line : input -> string
(** Read a LF or CRLF terminated string. *)



val write_byte : 'a output -> int -> unit
(** Write an unsigned 8-bit byte. *)

val write_c_string : 'a output -> string -> unit
(** Write a string and append an null character. *)

val write_c_bytes : 'a output -> bytes -> unit
(** Write a byte sequence and append an null character. *)

val write_line : 'a output -> string -> unit
(** Write a line and append a LF (it might be converted
  to CRLF on some systems depending on the underlying IO). *)



exception Overflow of string
(** Exception raised when a read or write operation cannot be completed. *)

module LittleEndian :
sig

val read_ui16 : input -> int
(** Read an unsigned 16-bit word. *)

val read_i16 : input -> int
(** Read a signed 16-bit word. *)

val read_i32 : input -> int
(** Read a signed 32-bit integer. Raise [Overflow] if the
  read integer cannot be represented as a Caml 31-bit integer. *)

(*
val read_real_i32 : input -> int32
(** Read a signed 32-bit integer as an OCaml int32. *)

val read_i64 : input -> int64
(** Read a signed 64-bit integer as an OCaml int64. *)

val read_float32 : input -> float
(** Read an IEEE single precision floating point value (32 bits). *)

val read_double : input -> float
(** Read an IEEE double precision floating point value (64 bits). *)
*)


val write_ui16 : 'a output -> int -> unit
(** Write an unsigned 16-bit word. *)

val write_i16 : 'a output -> int -> unit
(** Write a signed 16-bit word. *)

val write_i32 : 'a output -> int -> unit
(** Write a signed 32-bit integer. *)

(*
val write_real_i32 : 'a output -> int32 -> unit
(** Write an OCaml int32. *)

val write_i64 : 'a output -> int64 -> unit
(** Write an OCaml int64. *)

val write_float32 : 'a output -> float -> unit
(** Write an IEEE single precision floating point value (32 bits). *)

val write_double : 'a output -> float -> unit
(** Write an IEEE double precision floating point value (64 bits). *)
*)

end

(** Same as operations above, but use big-endian encoding *)
module BigEndian :
sig

  val read_ui16 : input -> int
  val read_i16 : input -> int
  val read_i32 : input -> int
  (*
  val read_real_i32 : input -> int32
  val read_i64 : input -> int64
  val read_float32 : input -> float
  val read_double : input -> float
  *)

  val write_ui16 : 'a output -> int -> unit
  val write_i16 : 'a output -> int -> unit
  val write_i32 : 'a output -> int -> unit
  (*
  val write_real_i32 : 'a output -> int32 -> unit
  val write_i64 : 'a output -> int64 -> unit
  val write_float32 : 'a output -> float -> unit
  val write_double : 'a output -> float -> unit
  *)

end

(** {6 Bits API}

  This enable you to read and write from an IO bit-by-bit or several bits
  at the same time.
*)

type in_bits
type out_bits

exception Bits_error

val input_bits : input -> in_bits
(** Read bits from an input *)

val output_bits : 'a output -> out_bits
(** Write bits to an output *)

val read_bits : in_bits -> int -> int
(** Read up to 31 bits, raise Bits_error if n < 0 or n > 31 *)

(* orig: second param was ~nbit *)
val write_bits : out_bits -> int -> int -> unit
(** Write up to 31 bits represented as a value, raise Bits_error if nbits < 0
 or nbits > 31 or the value representation excess nbits. *)

val flush_bits : out_bits -> unit
(** Flush remaining unwritten bits, adding up to 7 bits which values 0. *)

val drop_bits : in_bits -> unit
(** Drop up to 7 buffered bits and restart to next input character. *)
