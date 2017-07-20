
(** The type for file-system stat information. *)
type stat_info = {
  ctime: time;
  mtime: time;
  dev  : Int32.t;
  inode: Int32.t;
  mode : mode;
  uid  : Int32.t;
  gid  : Int32.t;
  size : Int32.t;
}
  and mode =
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
  stats : stat_info;
  id    : Blob.hash;
  stage : int;
  (* relative path *)
  name  : Common.filename;
}

(* the entries are sorted *)
type t = entry list

val empty: t

val entry_of_stat: Unix.stats -> Common.filename -> Sha1.t -> entry

val read: IO.input -> t
(* will write the header, and sha checksum at the end *)
val write: t -> unit IO.output -> unit

val remove: t -> Common.filename -> t
val add: t -> entry -> t
