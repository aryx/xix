(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * less: normalize filenames? use realpath?
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* global line number, after pre-processing *)
type loc = int
(* final readable location *)
type final_loc = Common.filename * int

type location_history = {
  location_event: location_event;
  global_line: loc;
}
  and location_event =
    (* #include "foo.h" *)
    | Include of Common.filename
    (* #line 1 "foo.c" *)
    | Line of int * Common.filename
    (* end of #include, back to includer *)
    | Eof

let history = ref []

(* Global line number (after pre-processing).
 * Note that you need another data structure to map a global line number 
 * to a (file, line) pair (see history and final_loc_of_loc below).
 *)
let line = ref 1


exception Error of string * loc

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let add_event event =
  history := {location_event = event; global_line = !line }::!history

(* for 5c -f *)
let dump_event event =
  match event with
  | Include file -> 
      pr (spf "%4d: %s" !line file)
  | Line (local_line, file) -> 
      pr (spf "%4d: %s (#line %d)" !line file local_line)
  | Eof -> 
      pr (spf "%4d: <pop>" !line)


(* 'history' contains the list of location_events in reverse 
 * (because we always add an event to the end), for instance: 
 * [200; 150; 130; 60; 1]. The first step is to reverse this list:
 * [1; 60; 130; 150; 200]. The, if we look for information about line 135, 
 * we want to stop when we encounter 150,
 * so when lineno < x.global_line below succeed for the first time.
 *)
let final_loc_of_loc lineno =
  let rec aux (lastfile, lastlineno, lastdelta) stack xs =
    match xs with
    | [] -> lastfile, lineno - lastlineno + lastdelta
    | x::xs ->
      if lineno < x.global_line 
      then lastfile, lineno - lastlineno + lastdelta
      else
        (match x.location_event, stack with
        | Eof, [] -> 
            failwith (spf "could not find final location for lineno %d" lineno)
        | Eof, y::ys ->
            aux y ys xs
        | Include file, ys ->
            aux (file, x.global_line, 1)
              ((lastfile, lastlineno, lastdelta)::ys) xs
        | Line (line, file), [] ->
            failwith "impossible: #line should occur in a file"
        | Line (line, file), _y::ys ->
            aux (file, x.global_line, line) ys xs
        )
  in
  aux ("<nofile>", 0, 0) [] (List.rev !history)

let final_loc_and_includers_of_loc lineno =
  raise Todo
