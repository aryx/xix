(*s: Shell.mli *)

(*s: type [[Shell.caps]] *)
(* Need:
 *  - exec/fork/wait: obviously as we run a shell
 *  - env: for MKSHELL
 *)
type caps = < Cap.exec; Cap.fork; Cap.wait; Cap.env >
(*e: type [[Shell.caps]] *)

(*s: signature [[Shell.exec_recipe]] *)
val exec_recipe :
  < caps; .. > ->
  Shellenv.t ->
  string list (* shell arguments (e.g., ["-e"]) *) ->
  string list (* shell stdin lines (the recipe) *) ->
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
  string (* sh output *)
(*e: signature [[Shell.exec_pipecmd]] *)

(* internals *)
(*s: signature [[Shell.exec_shell]] *)
val exec_shell: <Cap.exec; Cap.env; ..> -> Shellenv.t -> string list -> string list ->
  unit
(*e: signature [[Shell.exec_shell]] *)
(*e: Shell.mli *)
