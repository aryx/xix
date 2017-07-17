
type t = bytes

(* assumes have already read the 'blob <size>\000' header from input *)
val read: IO.input -> t
