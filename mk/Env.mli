(*s: Env.mli *)
(*s: type [[Env.values]] *)
(* Content of variables (after full expansion and backquote resolution).
 * It should not contain any empty strings (but it can contain empty lists).
 *)
type values = string list
(*e: type [[Env.values]] *)

(*s: type [[Env.t]] *)
type t = {
  (* use Env.add_var to add a var (to check if it's ok) *)
  vars         : (string, values) Hashtbl.t;
  internal_vars: (string, values) Hashtbl.t;

  (* those vars can not be overriden by the mkfile *)
  vars_commandline: (string, bool) Hashtbl.t;
  vars_we_set: (string, bool) Hashtbl.t;
}
(*e: type [[Env.t]] *)

(*s: exception [[Env.Redefinition]] *)
exception Redefinition of string
(*e: exception [[Env.Redefinition]] *)

(*s: signature [[Env.add_var]] *)
val add_var : t -> string -> values -> unit
(*e: signature [[Env.add_var]] *)

(*s: signature [[Env.initenv]] *)
(* Will read the OS environment variables (e.g., PATH, HOME, objtype).
 * Need also Cap.argv to set MKFLAGS
 *)
val initenv : < Cap.env ; Cap.argv; .. > -> t
(*e: signature [[Env.initenv]] *)
(*s: signature [[Env.shellenv_of_env]] *)
val shellenv_of_env : t -> Shellenv.t
(*e: signature [[Env.shellenv_of_env]] *)

(* internals *)

(*s: signature [[Env.check_values]] *)
val check_values : values -> unit
(*e: signature [[Env.check_values]] *)
(*s: signature [[Env.dump_env]] *)
val dump_env : t -> unit
(*e: signature [[Env.dump_env]] *)
(*e: Env.mli *)
