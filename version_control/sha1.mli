(* a 20 bytes number (really a string of length 20) *)
type t = bytes

(* computes SHA1 of a series of bytes *) 
val sha1: bytes -> t

val read: IO.input -> t
val write: 'a IO.output -> t -> unit

val is_sha: t -> bool
