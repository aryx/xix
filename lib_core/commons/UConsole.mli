(* See Console.mli for more information
 *
 * DO NOT USE THIS FILE! You should use CapConsole.mli instead.
 *)

(* Print a string, print a newline, and flush the stdout channel. *)
val print : string -> unit

(* Print a string and flush the stdout channel. *)
val print_no_nl : string -> unit

(* Print a string, print a newline, and flush the stderr channel.
 * You should avoid using this function; Prefer Logs.err or Logs.warn
 * in general.
 *)
val eprint : string -> unit
