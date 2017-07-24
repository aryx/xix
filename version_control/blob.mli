
type t = bytes

type hash = Sha1.t

(* assumes have already read the 'blob <size>\000' header from unzipped input *)
val read: IO.input -> t
(* does not write the header, does not compress *)
val write: t -> bytes IO.output -> unit

val show: t -> unit
