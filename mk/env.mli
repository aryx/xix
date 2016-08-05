
type values = string list

type t = {
  vars:          (string, values) Hashtbl.t;

  internal_vars: (string, bool) Hashtbl.t;
  vars_we_set: (string, bool) Hashtbl.t;
}

(* will read the OS environment variables (e.g., PATH, HOME, objtype) *)
val initenv: unit -> t
