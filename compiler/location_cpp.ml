(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* global line number, after pre-processing *)
type loc = int
(* final readable location *)
type final_loc = Common.filename * int

(* We could have the global 'line' below defined here instead of in globals.ml.
 * However, we can also call the C parser after cpp, in which
 * case the parser has nothing to do with the preprocessor,
 * but the parser still needs to manage a line number, so it is better to put
 * 'line' in globals.ml.
 * 
 * let line = ref 1
 *)

type location_history = {
  location_event: location_event;
  global_line: loc;
}
  and location_event =
    (* #include "foo.h" *)
    | Include of Common.filename
    (* #line 1 "foo.c" *)
    | Line of Common.filename * int
    (* end of #include, back to includer *)
    | Eof

let history = ref []

(* Global line number (after pre-processing).
 * Note that you need another data structure to map a global line number 
 * to a (file, line) pair (see Preprocessor.line_history).
 *)
let line = ref 1

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let add_event event =
  history := event::!history

let final_loc_of_loc lineno =
  raise Todo
