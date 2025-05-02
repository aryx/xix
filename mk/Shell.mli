
type caps = < Cap.fork ; Cap.exec ; Cap.env >

val exec_recipe :
  < caps; .. > ->
  Shellenv.t ->
  string list (* sh arguments *) ->
  string list (* sh stdin (recipe) *) ->
  bool (* interactive *) ->
  int (* pid *)

val exec_backquote :
  < caps; .. > ->
  Shellenv.t ->
  string (* sh stdin (recipe) *) ->
  string (* sh output *)

val exec_pipecmd :
  < caps ; .. > ->
  Shellenv.t ->
  string (* sh stdin (recipe) *) ->
  Common.filename (* sh output*)
