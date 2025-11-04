(*s: macroprocessor/Location_cpp.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Fpath_.Operators

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
(*s: type [[Location_cpp.loc]] *)
type loc = int
[@@deriving show]
(*e: type [[Location_cpp.loc]] *)

(* final readable location *)
(*s: type [[Location_cpp.final_loc]] *)
type final_loc = Fpath.t * int
[@@deriving show]
(*e: type [[Location_cpp.final_loc]] *)

(*s: type [[Location_cpp.location_history]] *)
type location_history = {
  location_event: location_event;
  global_line: loc;
}
(*e: type [[Location_cpp.location_history]] *)
(*s: type [[Location_cpp.location_event]] *)
  and location_event =
    (* #include "foo.h" *)
    | Include of Fpath.t
    (* #line 1 "foo.c" *)
    | Line of int * Fpath.t
    (* end of #include, back to includer *)
    | Eof
(*e: type [[Location_cpp.location_event]] *)
[@@deriving show {with_path = false}]

(* TODO? move to local in Parse_cpp.ml? *)
(*s: constant [[Location_cpp.history]] *)
let history = ref []
(*e: constant [[Location_cpp.history]] *)

(* Global line number (after pre-processing).
 * Note that you need another data structure to map a global line number 
 * to a (file, line) pair (see history and final_loc_of_loc below).
 *)
(*s: constant [[Location_cpp.line]] *)
let line = ref 1
(*e: constant [[Location_cpp.line]] *)

(*s: exception [[Location_cpp.Error]] *)
exception Error of string * loc
(*e: exception [[Location_cpp.Error]] *)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(*s: function [[Location_cpp.dump_event]] *)
(* for 5c -f 
 * alt: just rely on deriving show but kept this for compatibility with kencc
 *)
let dump_event (event : location_event) : unit =
  match event with
  | Include file -> 
      Logs.app (fun m -> m "%4d: %s" !line !!file)
  | Line (local_line, file) -> 
      Logs.app (fun m -> m "%4d: %s (#line %d)" !line !!file local_line)
  | Eof -> 
      Logs.app (fun m -> m "%4d: <pop>" !line)
(*e: function [[Location_cpp.dump_event]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Location_cpp.add_event]] *)
let add_event (event : location_event) : unit =
  (* alt: use Logs.debug instead of a flag *)
  if !Flags_cpp.debug_line
  then dump_event event;
  history := {location_event = event; global_line = !line }::!history
(*e: function [[Location_cpp.add_event]] *)


(*s: function [[Location_cpp.final_loc_of_loc]] *)
(* 'history' contains the list of location_events in reverse order
 * since we always add an event to the end, for instance: 
 * [200; 150; 130; 60; 1]. The first step is to reverse this list:
 * [1; 60; 130; 150; 200]. Then, if we look for information about line 135, 
 * we want to stop when we encounter 150,
 * so when lineno < x.global_line below succeed for the first time.
 *)
let final_loc_of_loc (lineno : loc) : final_loc =
  let rec aux (lastfile, lastlineno, lastdelta) stack xs =
    match xs with
    | [] -> lastfile, lineno - lastlineno + lastdelta
    | x::xs ->
      if lineno < x.global_line 
      then lastfile, lineno - lastlineno + lastdelta
      else
        (match x.location_event, stack with
        | Eof, (lastfile, _lastlineno, lastdelta)::ys ->
            (* bugfix: wrong!! TODO *)
            aux (lastfile, x.global_line, lastdelta) ys xs
        | Eof, [] -> 
            failwith ("impossible: wrong location history, unpaired Eof")
        | Include file, ys ->
            aux (file, x.global_line, 1)
              ((lastfile, lastlineno, lastdelta)::ys) xs
        | Line (line, file), _y::ys ->
            aux (file, x.global_line, line) ys xs
        | Line (line, file), [] ->
            (* todo: wrong *)
            aux (file, x.global_line, line) [] xs
        )
  in
  aux (Fpath.v "<nofile>", 0, 0) [] (List.rev !history)
(*e: function [[Location_cpp.final_loc_of_loc]] *)

(*s: function [[Location_cpp._final_loc_and_includers_of_loc]] *)
let _final_loc_and_includers_of_loc _lineno =
  raise Todo
(*e: function [[Location_cpp._final_loc_and_includers_of_loc]] *)
(*e: macroprocessor/Location_cpp.ml *)
