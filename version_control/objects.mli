(*s: version_control/objects.mli *)
open Stdcompat (* for bytes *)

(*s: type [[Objects.t]] *)
type t = 
  | Blob   of Blob.t
  | Tree   of Tree.t
  | Commit of Commit.t
  (*s: [[Objects.t]] cases *)
  (*  | Tag of Tag.t *)
  (*e: [[Objects.t]] cases *)
(*e: type [[Objects.t]] *)

(*s: signature [[Objects.read]] *)
(* assumes input is in decompressed form *)
val read: IO.input -> t
(*e: signature [[Objects.read]] *)

(*s: signature [[Objects.write]] *)
(* will not compress, will return unserialized content for sha1 computation *)
val write: t -> bytes IO.output -> unit
(*e: signature [[Objects.write]] *)
(*e: version_control/objects.mli *)
