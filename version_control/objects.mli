
type t = 
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tree   of Tree.t

(* assumes input is in decompressed form *)
val read: IO.input -> t

val write: t -> unit IO.output -> unit
