
(* Load all the object files and split in code vs data.
 * Will also relocate branching instructions.
 * less: return also LineDirective info per file.
 *)
val load: 
  <Cap.open_in; .. > ->
  Fpath.t list ->
  (Chan.i -> 'instr Ast_asm.program * 'loc_history) ->
  'instr Arch.t ->
  'instr Types.code array * Types.data list * Types.symbol_table
