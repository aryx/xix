(*s: version_control/blob.mli *)

(*s: type Blob.t (version_control/blob.mli) *)
type t = bytes
(*e: type Blob.t (version_control/blob.mli) *)

(*s: type Blob.hash (version_control/blob.mli) *)
type hash = Sha1.t
(*e: type Blob.hash (version_control/blob.mli) *)

(*s: signature Blob.read *)
(* assumes have already read the 'blob <size>\000' header from unzipped input *)
val read: IO.input -> t
(* does not write the header, does not compress *)
(*e: signature Blob.read *)
(*s: signature Blob.write *)
(* does not write the header, does not compress *)
val write: t -> bytes IO.output -> unit
(*e: signature Blob.write *)

(*s: signature Blob.show *)
val show: t -> unit
(*e: signature Blob.show *)
(*e: version_control/blob.mli *)
