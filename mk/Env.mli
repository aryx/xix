type values = string list

type t = {
  (* use Env.add_var to populate vars (to check if it's ok) *)
  vars : (string, values) Hashtbl.t;
  internal_vars : (string, values) Hashtbl.t;
  vars_commandline : (string, bool) Hashtbl.t;
  vars_we_set : (string, bool) Hashtbl.t;
}

exception Redefinition of string

val add_var : t -> string -> values -> unit

(* will read the OS environment variables (e.g., PATH, HOME, objtype) *)
val initenv : < Cap.env ; .. > -> t
val shellenv_of_env : t -> Shellenv.t

(* internals *)

val check_values : values -> unit
val dump_env : t -> unit
