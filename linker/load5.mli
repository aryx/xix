
(* Will also relocate branching instructions.
 * less: return also LineDirective info per file.
 *)
val load: 
  Common.filename list -> 
  Types5.code array * Types5.data list * Types.symbol_table
