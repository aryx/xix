
type loc = int
[@@deriving show]
type final_loc = Fpath.t * int
[@@deriving show]

type location_history = {
  location_event: location_event;
  global_line: loc;
}
  and location_event =
    | Include of Fpath.t
    | Line of int * Fpath.t
    | Eof
[@@deriving show]

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
