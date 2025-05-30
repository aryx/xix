(*s: Shellenv.mli *)
(*s: type [[Shellenv.t (Shellenv.mli)]] *)
type t = (string * string list) list
(*e: type [[Shellenv.t (Shellenv.mli)]] *)

(*s: signature [[Shellenv.read_environment]] *)
val read_environment : < Cap.env ; .. > -> t
(*e: signature [[Shellenv.read_environment]] *)
(*e: Shellenv.mli *)
