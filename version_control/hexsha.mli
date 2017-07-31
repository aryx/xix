(* a 40 characters string, e.g. "d670460b4b4aece5915caf5c68d12f560a9fe3e4" *)
type t = string

val of_sha: Sha1.t -> t
val to_sha: t -> Sha1.t

val read: IO.input -> t
val write: 'a IO.output -> t -> unit

val is_hexsha: t -> bool

