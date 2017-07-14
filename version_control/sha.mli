open Common

(* a 20 bytes number (really a string of length 20) *)
type t = bytes

val sha1: bytes -> t

val is_sha: t -> bool
