(*s: Env.mli *)
type t = (string * string list) list

val read_environment : < Cap.env ; .. > -> t

(*e: Env.mli *)
