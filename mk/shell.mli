
val exec_recipe: 
  Shellenv.t -> string list (* sh arguments *) -> string list (* sh stdin *) ->
  bool (* interactive *) ->
  int (* pid *)

val exec_backquote:
  Shellenv.t -> string (* sh stdin *) -> string (* sh output *)
