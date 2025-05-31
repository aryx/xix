(*s: Env.mli *)
(*s: type [[Env.values (Env.mli)]] *)
type values = string list
(*e: type [[Env.values (Env.mli)]] *)

(*s: type [[Env.t (Env.mli)]] *)
type t = {
  (* use Env.add_var to populate vars (to check if it's ok) *)
  vars : (string, values) Hashtbl.t;
  internal_vars : (string, values) Hashtbl.t;
  vars_commandline : (string, bool) Hashtbl.t;
  vars_we_set : (string, bool) Hashtbl.t;
}
(*e: type [[Env.t (Env.mli)]] *)

(*s: exception [[Env.Redefinition (Env.mli)]] *)
exception Redefinition of string
(*e: exception [[Env.Redefinition (Env.mli)]] *)

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
