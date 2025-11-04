(*s: macroprocessor/Location_cpp.mli *)

(*s: type [[Location_cpp.loc (macroprocessor/Location_cpp.mli)]] *)
type loc = int
[@@deriving show]
(*e: type [[Location_cpp.loc (macroprocessor/Location_cpp.mli)]] *)
(*s: type [[Location_cpp.final_loc (macroprocessor/Location_cpp.mli)]] *)
type final_loc = Fpath.t * int
[@@deriving show]
(*e: type [[Location_cpp.final_loc (macroprocessor/Location_cpp.mli)]] *)

(*s: type [[Location_cpp.location_history (macroprocessor/Location_cpp.mli)]] *)
type location_history = {
  location_event: location_event;
  global_line: loc;
}
(*e: type [[Location_cpp.location_history (macroprocessor/Location_cpp.mli)]] *)
(*s: type [[Location_cpp.location_event (macroprocessor/Location_cpp.mli)]] *)
  and location_event =
    | Include of Fpath.t
    | Line of int * Fpath.t
    | Eof
(*e: type [[Location_cpp.location_event (macroprocessor/Location_cpp.mli)]] *)
[@@deriving show]

(*s: signature [[Location_cpp.history]] *)
(* both should be reseted each time you parse a new file *)
val history: location_history list ref
(*e: signature [[Location_cpp.history]] *)
(*s: signature [[Location_cpp.line]] *)
val line: loc ref
(*e: signature [[Location_cpp.line]] *)

(*s: exception [[Location_cpp.Error (macroprocessor/Location_cpp.mli)]] *)
exception Error of string * loc
(*e: exception [[Location_cpp.Error (macroprocessor/Location_cpp.mli)]] *)


(*s: signature [[Location_cpp.add_event]] *)
(* add to history *)
val add_event: 
  location_event -> unit
(*e: signature [[Location_cpp.add_event]] *)

(*s: signature [[Location_cpp.final_loc_of_loc]] *)
(* !uses history! you should avoid this function in a multifile processing
 * context (e.g., in the linker) *)
val final_loc_of_loc: 
    loc -> final_loc
(*e: signature [[Location_cpp.final_loc_of_loc]] *)


(*s: signature [[Location_cpp.dump_event]] *)
val dump_event:
  location_event -> unit
(*e: signature [[Location_cpp.dump_event]] *)
(*e: macroprocessor/Location_cpp.mli *)
