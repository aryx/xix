
(* will relocate branching instructions 
 * less: return also LineDirective info
 *)
val load: 
  string (* entry point *) -> Common.filename list -> 
  Types5.code array * Types5.data list * Types.symbol_table
