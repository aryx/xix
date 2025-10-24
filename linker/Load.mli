
(* Load all the object files and split in code vs data.
 * Will also relocate branching instructions.
 * less: return also LineDirective info per file.
 * The fpaths can be object files or library files!
 *)
val load: 
  <Cap.open_in; .. > ->
  Fpath.t list ->
  'instr Arch.t ->
  'instr Types.code array * Types.data list * Types.symbol_table
