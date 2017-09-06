(*s: version_control/repository.mli *)

(*s: type Repository.t (version_control/repository.mli) *)
type t = {
  worktree: Common.filename;
  dotgit: Common.filename;

  mutable index: Index.t;
}
(*e: type Repository.t (version_control/repository.mli) *)

(*s: type Repository.objectish (version_control/repository.mli) *)
type objectish =
  | ObjByRef of Refs.t
  | ObjByHex of Hexsha.t
(*e: type Repository.objectish (version_control/repository.mli) *)


(*s: signature Repository.init *)
(* repo *)
val init: Common.filename -> unit
(*e: signature Repository.init *)
(*s: signature Repository.open_ *)
val open_: Common.filename -> t
(*e: signature Repository.open_ *)

(*s: signature Repository.read_obj *)
(* objects *)
val read_obj: t -> Sha1.t -> Objects.t
(*e: signature Repository.read_obj *)
(*s: signature Repository.read_objectish *)
val read_objectish: t -> objectish -> Objects.t
(*e: signature Repository.read_objectish *)
(*s: signature Repository.read_commit *)
val read_commit: t -> Sha1.t -> Commit.t
(*e: signature Repository.read_commit *)
(*s: signature Repository.read_tree *)
val read_tree: t -> Sha1.t -> Tree.t
(*e: signature Repository.read_tree *)
(*s: signature Repository.read_blob *)
val read_blob: t -> Sha1.t -> Blob.t
(*e: signature Repository.read_blob *)
(*s: signature Repository.add_obj *)
val add_obj: t -> Objects.t -> Sha1.t
(*e: signature Repository.add_obj *)
(*s: signature Repository.has_obj *)
val has_obj: t -> Sha1.t -> bool
(*e: signature Repository.has_obj *)

(*s: signature Repository.read_ref *)
(* refs *)
val read_ref: t -> Refs.t -> Refs.ref_content
(*e: signature Repository.read_ref *)
(*s: signature Repository.write_ref *)
val write_ref: t -> Refs.t -> Refs.ref_content -> unit
(*e: signature Repository.write_ref *)
(*s: signature Repository.follow_ref *)
val follow_ref: t -> Refs.t -> Refs.t list * Commit.hash option
(*e: signature Repository.follow_ref *)
(*s: signature Repository.follow_ref_some *)
val follow_ref_some: t -> Refs.t -> Commit.hash
(*e: signature Repository.follow_ref_some *)
(*s: signature Repository.all_refs *)
val all_refs: t -> Refs.refname list
(* better than write_ref, will follow symbolic ref *)
(*e: signature Repository.all_refs *)
(*s: signature Repository.set_ref *)
(* better than write_ref, will follow symbolic ref *)
val set_ref: t -> Refs.t -> Commit.hash -> unit
(*e: signature Repository.set_ref *)
(*s: signature Repository.del_ref *)
val del_ref: t -> Refs.t -> unit
(* atomic op *)
(*e: signature Repository.del_ref *)
(*s: signature Repository.add_ref_if_new *)
(* atomic op *)
val add_ref_if_new: t -> Refs.t -> Refs.ref_content -> bool
(*e: signature Repository.add_ref_if_new *)
(*s: signature Repository.set_ref_if_same_old *)
val set_ref_if_same_old: t -> Refs.t -> Sha1.t -> Sha1.t -> bool
(*e: signature Repository.set_ref_if_same_old *)

(*s: signature Repository.read_index *)
(* index *)
val read_index: t -> Index.t
(*e: signature Repository.read_index *)
(*s: signature Repository.write_index *)
val write_index: t -> unit
(*e: signature Repository.write_index *)
(*s: signature Repository.add_in_index *)
val add_in_index: t -> Common.filename list -> unit
(*e: signature Repository.add_in_index *)

(*s: signature Repository.commit_index *)
val commit_index: 
  t -> User.t (* author *) -> User.t (* committer *) -> string (* msg *) -> unit
(*e: signature Repository.commit_index *)
(*s: signature Repository.set_worktree_and_index_to_tree *)
val set_worktree_and_index_to_tree:
  t -> Tree.t -> unit
(*e: signature Repository.set_worktree_and_index_to_tree *)

(* packs *)

(*s: signature Repository.walk_dir *)
(* misc *)
val walk_dir: 
  (Common.filename -> Common.filename list -> Common.filename list -> unit) ->
  Common.filename ->
  unit
(*e: signature Repository.walk_dir *)

(*e: version_control/repository.mli *)
