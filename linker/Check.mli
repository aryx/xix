
(* Make sure there is no reference to undefined symbols. 
 * raise Failure in case of error.
 *)
val check: 
  Types.symbol_table -> unit
