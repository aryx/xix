
val exec_recipe: 
  Shellenv.t -> 
  string list (* sh arguments *) -> string list (* sh stdin (recipe) *) ->
  bool (* interactive *) ->
  int (* pid *)

val exec_backquote:
  Shellenv.t -> string (* sh stdin (recipe) *) -> string (* sh output *)

val exec_pipecmd:
  Shellenv.t -> string (* sh stdin (recipe) *) -> Common.filename (* sh output*)
