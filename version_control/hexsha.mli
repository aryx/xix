
(* a 40 characters string, e.g. "d670460b4b4aece5915caf5c68d12f560a9fe3e4" *)
type t = string

val of_sha: Sha.t -> t
val to_sha: t -> Sha.t

val is_hexsha: t -> bool
