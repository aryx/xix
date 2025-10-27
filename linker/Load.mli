
(* Load all the object (and library) files and split in code vs data.
 * Will also relocate branching instructions.
 * less: return also LineDirective info per file.
 *)
val load: 
  <Cap.open_in; .. > ->
  Fpath.t list ->
  'instr Arch_linker.t ->
  'instr Types.code array * Types.data list * Types.symbol_table
