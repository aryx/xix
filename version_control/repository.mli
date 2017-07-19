
type t = {
  worktree: Common.filename;
  dotgit: Common.filename;
}

(* repo *)
val init: Common.filename -> unit
val clone: t -> Common.filename -> unit
val open_: Common.filename -> t

(* objects *)
val read_obj: t -> Sha1.t -> Objects.t
val mem_obj: t -> Sha1.t -> bool
val write_obj: t -> Sha1.t -> Objects.t -> unit

(* refs *)
val read_ref: t -> Refs.t -> Refs.ref_content
val remove_ref: t -> Refs.t -> unit
val add_ref: t -> Refs.t -> Refs.ref_content -> unit
val follow_ref: t -> Refs.t -> Commit.hash
(*val test_and_set_ref: t -> Refs.t ->  *)

(* index *)
val read_index: t -> Index.t
val write_index: t -> Index.t -> unit

(* packs *)
