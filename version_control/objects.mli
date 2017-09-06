(*s: version_control/objects.mli *)

(*s: type Objects.t (version_control/objects.mli) *)
type t = 
  | Blob   of Blob.t
  | Commit of Commit.t
  | Tree   of Tree.t
(*e: type Objects.t (version_control/objects.mli) *)

(*s: signature Objects.read *)
(* assumes input is in decompressed form *)
val read: IO.input -> t
(*e: signature Objects.read *)

(*s: signature Objects.write *)
(* will not compress, will return unserialized content for sha1 computation *)
val write: t -> bytes IO.output -> unit
(*e: signature Objects.write *)
(*e: version_control/objects.mli *)
