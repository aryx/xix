
val exec_recipe:
  < Cap.fork; Cap.exec; .. > ->
  Shellenv.t -> 
  string list (* sh arguments *) -> string list (* sh stdin (recipe) *) ->
  bool (* interactive *) ->
  int (* pid *)

val exec_backquote:
  < Cap.fork; Cap.exec; .. > ->
  Shellenv.t -> string (* sh stdin (recipe) *) -> string (* sh output *)

val exec_pipecmd:
  < Cap.fork; Cap.exec; .. > ->
  Shellenv.t -> string (* sh stdin (recipe) *) -> Common.filename (* sh output*)
