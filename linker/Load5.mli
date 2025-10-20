
(* Load all the object files and split in code vs data.
 * Will also relocate branching instructions.
 * less: return also LineDirective info per file.
 *)
val load: 
  <Cap.open_in; .. > ->
  Fpath.t list -> 
  Ast_asm5.instr_with_cond Types.code array * Types.data list * Types.symbol_table
