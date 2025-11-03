(*s: Check.mli *)

(*s: signature [[Check.check]] *)
(* Make sure there is no reference to undefined symbols. 
 * raise Failure in case of error.
 *)
val check: 
  Types.symbol_table -> unit
(*e: signature [[Check.check]] *)
(*e: Check.mli *)
