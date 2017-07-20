
type t = 
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tree   of Tree.t

(* assumes input is in decompressed form *)
val read: IO.input -> t

(* will not compress, will return unserialized content for sha1 computation *)
val write: t -> bytes IO.output -> unit
