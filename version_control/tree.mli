(*s: version_control/tree.mli *)

(*s: type Tree.perm (version_control/tree.mli) *)
type perm = 
  | Normal
  | Exec
  | Link
  | Dir
  | Commit (* submodule? *)
(*e: type Tree.perm (version_control/tree.mli) *)

(*s: type Tree.entry (version_control/tree.mli) *)
type entry = {
  perm: perm;
  name: string;
  (* blob or tree *)
  node: Sha1.t;
}
(*e: type Tree.entry (version_control/tree.mli) *)

(*s: type Tree.t (version_control/tree.mli) *)
type t = entry list
(*e: type Tree.t (version_control/tree.mli) *)

(*s: type Tree.hash (version_control/tree.mli) *)
type hash = Sha1.t
(*e: type Tree.hash (version_control/tree.mli) *)


(*s: signature Tree.read *)
(* assumes have already read the 'tree <size>\000' header from unzipped input *)
val read: IO.input -> t
(* does not write the header, does not compress *)
(*e: signature Tree.read *)
(*s: signature Tree.write *)
(* does not write the header, does not compress *)
val write: t -> bytes IO.output -> unit
(*e: signature Tree.write *)

(*s: signature Tree.show *)
val show: t -> unit
(*e: signature Tree.show *)

(*s: signature Tree.walk_tree *)
val walk_tree: 
  (hash -> t) -> Common.filename (* dir *) -> 
  (Common.filename -> entry -> unit) -> t -> unit
(*e: signature Tree.walk_tree *)

(*s: signature Tree.walk_trees *)
val walk_trees:
  (hash -> t) -> Common.filename (* dir *) ->
  (Common.filename -> entry option -> entry option -> unit) -> t -> t -> unit
(*e: signature Tree.walk_trees *)

(*e: version_control/tree.mli *)
