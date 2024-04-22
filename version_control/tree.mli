(*s: version_control/tree.mli *)
open Stdcompat (* for bytes *)

(*s: type [[Tree.perm]] *)
(* similar to Index.mode, but with also a 'Dir' *)
type perm = 
  | Normal
  | Dir
  | Exec
  (*s: [[Tree.perm]] cases *)
  | Link
  (*x: [[Tree.perm]] cases *)
  | Commit (* ?? submodule? *)
  (*e: [[Tree.perm]] cases *)
(*e: type [[Tree.perm]] *)

(*s: type [[Tree.entry]] *)
type entry = {
  (* relative to tree, so does not contain any '/', or '.' or '..' *)
  name: string;
  (* Blob.hash or Tree.hash *)
  id: Sha1.t;

  perm: perm;
}
(*e: type [[Tree.entry]] *)

(*s: type [[Tree.t]] *)
(* todo: entries must be sorted! and each name must be unique *)
type t = entry list
(*e: type [[Tree.t]] *)

(*s: type [[Tree.hash]] *)
type hash = Sha1.t
(*e: type [[Tree.hash]] *)


(*s: signature [[Tree.read]] *)
(* assumes have already read the 'tree <size>\000' header from unzipped input *)
val read: IO.input -> t
(*e: signature [[Tree.read]] *)
(*s: signature [[Tree.write]] *)
(* does not write the header, does not compress *)
val write: t -> bytes IO.output -> unit
(*e: signature [[Tree.write]] *)

(*s: signature [[Tree.show]] *)
val show: t -> unit
(*e: signature [[Tree.show]] *)

(*s: signature [[Tree.walk_tree]] *)
val walk_tree: 
  (hash -> t) -> Common.filename (* dir *) -> 
  (Common.filename -> entry -> unit) -> t -> unit
(*e: signature [[Tree.walk_tree]] *)

(*s: signature [[Tree.walk_trees]] *)
val walk_trees:
  (hash -> t) -> Common.filename (* dir *) ->
  (Common.filename -> entry option -> entry option -> unit) -> t -> t -> unit
(*e: signature [[Tree.walk_trees]] *)

(*e: version_control/tree.mli *)
