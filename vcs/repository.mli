(*s: version_control/repository.mli *)

(*s: type [[Repository.t]] *)
type t = {
  worktree: Fpath.t;
  dotgit: Fpath.t; (* usually <worktree>/.git *)

  (*s: [[Repository.t]] index field *)
  mutable index: Index.t;
  (*e: [[Repository.t]] index field *)
}
(*e: type [[Repository.t]] *)

(*s: type [[Repository.objectish]] *)
(* todo: handle ^ like HEAD^, so need more complex objectish parser *)
type objectish =
  | ObjByRef of Refs.t
  | ObjByHex of Hexsha.t
  | ObjByBranch of string
  (*s: [[Repository.objectish]] cases *)
  (* todo:  ObjByShortHex *)
  (*x: [[Repository.objectish]] cases *)
  (* ObjByTag *)
  (*e: [[Repository.objectish]] cases *)
(*e: type [[Repository.objectish]] *)


(* repo *)
(*s: signature [[Repository.init]] *)
val init: < Cap.stdout; Cap.chdir; Cap.open_out; Cap.open_in; ..> -> 
  Fpath.t -> unit
(*e: signature [[Repository.init]] *)
(*s: signature [[Repository.open_]] *)
val open_: < Cap.open_in; ..> ->
  Fpath.t -> t
(*e: signature [[Repository.open_]] *)
(*s: signature [[Repository.find_dotgit_root_and_open]] *)
val find_root_open_and_adjust_paths: < Cap.open_in; ..> ->
 Fpath.t list -> t * Fpath.t list
(*e: signature [[Repository.find_dotgit_root_and_open]] *)

(* objects *)
(*s: signature [[Repository.read_obj]] *)
val read_obj: < Cap.open_in; ..> ->
  t -> Sha1.t -> Objects.t
(*e: signature [[Repository.read_obj]] *)
(*s: signature [[Repository.read_objectish]] *)
val read_objectish: <Cap.open_in; ..> -> 
  t -> objectish -> Sha1.t * Objects.t
(*e: signature [[Repository.read_objectish]] *)
(*s: signature [[Repository.read_commit]] *)
val read_commit: < Cap.open_in; ..> ->
  t -> Sha1.t -> Commit.t
(*e: signature [[Repository.read_commit]] *)
(*s: signature [[Repository.read_tree]] *)
val read_tree: < Cap.open_in; ..> ->
  t -> Sha1.t -> Tree.t
(*e: signature [[Repository.read_tree]] *)
(*s: signature [[Repository.read_blob]] *)
val read_blob: < Cap.open_in; ..> ->
  t -> Sha1.t -> Blob.t
(*e: signature [[Repository.read_blob]] *)
(*s: signature [[Repository.add_obj]] *)
val add_obj: t -> Objects.t -> Sha1.t
(*e: signature [[Repository.add_obj]] *)
(*s: signature [[Repository.has_obj]] *)
val has_obj: t -> Sha1.t -> bool
(*e: signature [[Repository.has_obj]] *)

(* refs *)
(*s: signature [[Repository.read_ref]] *)
val read_ref: <Cap.open_in; ..> ->
  t -> Refs.t -> Refs.ref_content
(*e: signature [[Repository.read_ref]] *)
(*s: signature [[Repository.write_ref]] *)
val write_ref: t -> Refs.t -> Refs.ref_content -> unit
(*e: signature [[Repository.write_ref]] *)
(*s: signature [[Repository.follow_ref]] *)
val follow_ref: <Cap.open_in; ..> ->
  t -> Refs.t -> Refs.t list * Commit.hash option
(*e: signature [[Repository.follow_ref]] *)
(*s: signature [[Repository.follow_ref_some]] *)
val follow_ref_some: <Cap.open_in; ..> ->
  t -> Refs.t -> Commit.hash
(*e: signature [[Repository.follow_ref_some]] *)
(*s: signature [[Repository.all_refs]] *)
val all_refs: t -> Refs.refname list
(*e: signature [[Repository.all_refs]] *)
(*s: signature [[Repository.set_ref]] *)
(* better than write_ref, will follow symbolic ref *)
val set_ref: < Cap.open_in; ..> ->
  t -> Refs.t -> Commit.hash -> unit
(*e: signature [[Repository.set_ref]] *)
(*s: signature [[Repository.del_ref]] *)
val del_ref: <Cap.open_out; ..> -> 
  t -> Refs.t -> unit
(*e: signature [[Repository.del_ref]] *)
(* atomic op *)
(*s: signature [[Repository.add_ref_if_new]] *)
val add_ref_if_new: < Cap.open_out; Cap.open_in; .. > ->
  t -> Refs.t -> Refs.ref_content -> bool
(*e: signature [[Repository.add_ref_if_new]] *)
(*s: signature [[Repository.set_ref_if_same_old]] *)
val set_ref_if_same_old: < Cap.open_out; Cap.open_in; .. > ->
  t -> Refs.t -> Sha1.t -> Sha1.t -> bool
(*e: signature [[Repository.set_ref_if_same_old]] *)

(* index *)
(*s: signature [[Repository.read_index]] *)
val read_index: t -> Index.t
(*e: signature [[Repository.read_index]] *)
(*s: signature [[Repository.write_index]] *)
val write_index: t -> unit
(*e: signature [[Repository.write_index]] *)
(*s: signature [[Repository.add_in_index]] *)
val add_in_index: < Cap.open_in; ..> ->
  t -> Fpath.t list -> unit
(*e: signature [[Repository.add_in_index]] *)

(*s: signature [[Repository.commit_index]] *)
val commit_index: < Cap.open_in; .. > ->
  t -> User.t (* author *) -> User.t (* committer *) -> string (* msg *) -> unit
(*e: signature [[Repository.commit_index]] *)
(*s: signature [[Repository.set_worktree_and_index_to_tree]] *)
val set_worktree_and_index_to_tree: < Cap.open_out; Cap.open_in; ..> ->
  t -> Tree.t -> unit
(*e: signature [[Repository.set_worktree_and_index_to_tree]] *)

(* packs *)

(* misc *)
(*s: signature [[Repository.walk_dir]] *)
val walk_dir: 
  (Fpath.t (* dir *) -> string (* subdir *) list -> string (* file *) list -> unit) ->
  Fpath.t ->
  unit
(*e: signature [[Repository.walk_dir]] *)

val parse_objectish: string -> objectish

(*e: version_control/repository.mli *)
