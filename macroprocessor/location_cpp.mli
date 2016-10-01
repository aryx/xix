
type loc = int
type final_loc = Common.filename * int

type location_history = {
  location_event: location_event;
  global_line: loc;
}
  and location_event =
    | Include of Common.filename
    | Line of int * Common.filename
    | Eof

(* both should be reseted each time you parse a new file *)
val history: location_history list ref
val line: loc ref

exception Error of string * loc


(* add to history *)
val add_event: 
  location_event -> unit

(* uses history *)
val final_loc_of_loc: 
    loc -> final_loc


val dump_event:
  location_event -> unit
