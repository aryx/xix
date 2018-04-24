(*s: version_control/index.mli *)

(*s: type [[Index.stat_info]] *)
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
(*e: type [[Index.stat_info]] *)
(*s: type [[Index.mode]] *)
and mode =
  (* no directory here *)
  | Normal
  | Exec
  (*s: [[Index.mode]] cases *)
  | Link
  (*x: [[Index.mode]] cases *)
  | Gitlink (*?? submodule? *)
  (*e: [[Index.mode]] cases *)
(*e: type [[Index.mode]] *)
(*s: type [[Index.time]] *)
(** The type for a time represented by its [lsb32] and [nsec] parts. *)
and time = {
  lsb32: Int32.t;
  nsec : Int32.t;
}
(*e: type [[Index.time]] *)
    
(*s: type [[Index.entry]] *)
(** The type for a Git index entry. *)
type entry = {
  (* relative path *)
  path  : Common.filename;
  id    : Blob.hash;

  stats : stat_info;
}
(*e: type [[Index.entry]] *)

(*s: type [[Index.t]] *)
(* the entries are sorted (see compare_entries below) *)
type t = entry list
(*e: type [[Index.t]] *)

(*s: signature [[Index.empty]] *)
val empty: t
(*e: signature [[Index.empty]] *)
(*s: signature [[Index.mk_entry]] *)
val mk_entry: Common.filename -> Sha1.t -> Unix.stats -> entry
(*e: signature [[Index.mk_entry]] *)

(*s: signature [[Index.stat_info_of_lstats]] *)
val stat_info_of_lstats: Unix.stats -> stat_info
(*e: signature [[Index.stat_info_of_lstats]] *)
(*s: signature [[Index.mode_of_perm]] *)
val mode_of_perm: Tree.perm -> mode
(*e: signature [[Index.mode_of_perm]] *)
(*s: signature [[Index.perm_of_mode]] *)
val perm_of_mode: mode -> Tree.perm
(*e: signature [[Index.perm_of_mode]] *)

(*s: signature [[Index.read]] *)
val read: IO.input -> t
(*e: signature [[Index.read]] *)
(*s: signature [[Index.write]] *)
(* will write the header, and sha checksum at the end *)
val write: t -> unit IO.output -> unit
(*e: signature [[Index.write]] *)

(*s: signature [[Index.remove_entry]] *)
val remove_entry: t -> Common.filename -> t
(*e: signature [[Index.remove_entry]] *)
(*s: signature [[Index.add_entry]] *)
val add_entry: t -> entry -> t
(*e: signature [[Index.add_entry]] *)

(*s: signature [[Index.trees_of_index]] *)
val trees_of_index: t -> (* add_obj *)(Tree.t -> Tree.hash) -> Tree.hash
(*e: signature [[Index.trees_of_index]] *)
(*e: version_control/index.mli *)
