(*s: version_control/hexsha.mli *)
(*s: type [[Hexsha.t]] *)
(* a 40 characters string, e.g. "d670460b4b4aece5915caf5c68d12f560a9fe3e4" *)
type t = string
(*e: type [[Hexsha.t]] *)

(*s: signature [[Hexsha.of_sha]] *)
val of_sha: Sha1.t -> t
(*e: signature [[Hexsha.of_sha]] *)
(*s: signature [[Hexsha.to_sha]] *)
val to_sha: t -> Sha1.t
(*e: signature [[Hexsha.to_sha]] *)

(*s: signature [[Hexsha.read]] *)
val read: IO.input -> t
(*e: signature [[Hexsha.read]] *)
(*s: signature [[Hexsha.write]] *)
val write: 'a IO.output -> t -> unit
(*e: signature [[Hexsha.write]] *)

(*s: signature [[Hexsha.is_hexsha]] *)
val is_hexsha: t -> bool
(*e: signature [[Hexsha.is_hexsha]] *)

(*e: version_control/hexsha.mli *)
