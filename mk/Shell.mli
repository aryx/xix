(*s: Shell.mli *)

(*s: type [[Shell.caps (Shell.mli)]] *)
type caps = < Cap.fork ; Cap.exec ; Cap.env >
(*e: type [[Shell.caps (Shell.mli)]] *)

(*s: signature [[Shell.exec_recipe]] *)
val exec_recipe :
  < caps; .. > ->
  Shellenv.t ->
  string list (* sh arguments *) ->
  string list (* sh stdin (recipe) *) ->
  bool (* interactive *) ->
  int (* pid *)
(*e: signature [[Shell.exec_recipe]] *)

(*s: signature [[Shell.exec_backquote]] *)
val exec_backquote :
  < caps; .. > ->
  Shellenv.t ->
  string (* sh stdin (recipe) *) ->
  string (* sh output *)
(*e: signature [[Shell.exec_backquote]] *)

(*s: signature [[Shell.exec_pipecmd]] *)
val exec_pipecmd :
  < caps ; .. > ->
  Shellenv.t ->
  string (* sh stdin (recipe) *) ->
  Common.filename (* sh output*)
(*e: signature [[Shell.exec_pipecmd]] *)
(*e: Shell.mli *)
