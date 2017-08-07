
type t = {
  worktree: Common.filename;
  dotgit: Common.filename;

  mutable index: Index.t;
}

type objectish =
  | ObjByRef of Refs.t
  | ObjByHex of Hexsha.t


(* repo *)
val init: Common.filename -> unit
val open_: Common.filename -> t

(* objects *)
val read_obj: t -> Sha1.t -> Objects.t
val read_objectish: t -> objectish -> Objects.t
val read_commit: t -> Sha1.t -> Commit.t
val read_tree: t -> Sha1.t -> Tree.t
val read_blob: t -> Sha1.t -> Blob.t
val add_obj: t -> Objects.t -> Sha1.t
val has_obj: t -> Sha1.t -> bool

(* refs *)
val read_ref: t -> Refs.t -> Refs.ref_content
val write_ref: t -> Refs.t -> Refs.ref_content -> unit
val follow_ref: t -> Refs.t -> Refs.t list * Commit.hash option
val follow_ref_some: t -> Refs.t -> Commit.hash
val all_refs: t -> Refs.refname list
(* better than write_ref, will follow symbolic ref *)
val set_ref: t -> Refs.t -> Commit.hash -> unit
val del_ref: t -> Refs.t -> unit
(* atomic op *)
val add_ref_if_new: t -> Refs.t -> Refs.ref_content -> bool
val set_ref_if_same_old: t -> Refs.t -> Sha1.t -> Sha1.t -> bool

(* index *)
val read_index: t -> Index.t
val write_index: t -> unit
val add_in_index: t -> Common.filename list -> unit

val commit_index: 
  t -> User.t (* author *) -> User.t (* committer *) -> string (* msg *) -> unit
val set_worktree_and_index_to_tree:
  t -> Tree.t -> unit

(* packs *)

(* misc *)
val walk_dir: 
  (Common.filename -> Common.filename list -> Common.filename list -> unit) ->
  Common.filename ->
  unit

