
type t = {
  worktree: Common.filename;
  dotgit: Common.filename;

  mutable index: Index.t;
}

(* repo *)
val init: Common.filename -> unit
val clone: t -> Common.filename -> unit
val open_: Common.filename -> t

(* objects *)
val read_obj: t -> Sha1.t -> Objects.t
val mem_obj: t -> Sha1.t -> bool
val add_obj: t -> Objects.t -> Sha1.t

(* refs *)
val read_ref: t -> Refs.t -> Refs.ref_content
val remove_ref: t -> Refs.t -> unit
val add_ref: t -> Refs.t -> Refs.ref_content -> unit
val resolve_ref: t -> Refs.t -> Commit.hash
(*val test_and_set_ref: t -> Refs.t ->  *)

(* index *)
val read_index: t -> Index.t
val write_index: t -> unit
val add_in_index: t -> Common.filename list -> unit

val commit: t -> User.t -> User.t -> string -> unit

(* packs *)


