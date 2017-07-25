
(** The type for file-system stat information. *)
type stat_info = {
  mode : mode;
  ctime: time;
  mtime: time;
  dev  : Int32.t;
  inode: Int32.t;
  uid  : Int32.t;
  gid  : Int32.t;
  size : Int32.t;
}
  and mode =
    (* no directory here *)
    | Normal
    | Exec
    | Link
    | Gitlink
  (** The type for a time represented by its [lsb32] and [nsec] parts. *)
  and time = {
    lsb32: Int32.t;
    nsec : Int32.t;
  }
    
(** The type for a Git index entry. *)
type entry = {
  (* relative path *)
  name  : Common.filename;
  id    : Blob.hash;
  stats : stat_info;
  stage : int;
}

(* the entries are sorted *)
type t = entry list

val empty: t
val mk_entry: Common.filename -> Sha1.t -> Unix.stats -> entry

val read: IO.input -> t
(* will write the header, and sha checksum at the end *)
val write: t -> unit IO.output -> unit

val remove_entry: t -> Common.filename -> t
val add_entry: t -> entry -> t

val tree_of_index: t -> (* add_obj *)(Tree.t -> Tree.hash) -> Tree.hash
(* todo: index_of_tree *)
