(*s: version_control/sha1.mli *)

(*s: type [[Sha1.t]] *)
(* a 20 bytes number (really a string of length 20) *)
type t = string
(*e: type [[Sha1.t]] *)

(*s: signature [[Sha1.sha1]] *)
(* computes SHA1 of a series of bytes *) 
val sha1: string -> t
(*e: signature [[Sha1.sha1]] *)

(*s: signature [[Sha1.read]] *)
val read: IO.input -> t
(*e: signature [[Sha1.read]] *)
(*s: signature [[Sha1.write]] *)
val write: 'a IO.output -> t -> unit
(*e: signature [[Sha1.write]] *)

(*s: signature [[Sha1.is_sha]] *)
val is_sha: t -> bool
(*e: signature [[Sha1.is_sha]] *)
(*e: version_control/sha1.mli *)
