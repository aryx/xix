(* Partial port of the Plan9 API to OCaml (Similar to unix.ml, but for Plan9) *)
open Common

(* uniQue file identifier (fileserver entity) *)
type qid = {
  path: int64;
  vers: int;
  typ: qid_type;
}
  and qid_type =
    | QTFile
    | QTDir

(* read/write/execute *)
type perm_property = { r: bool; w: bool; x: bool }

val r: perm_property
val w: perm_property
val rw: perm_property
val rx: perm_property
val rwx: perm_property

type dir_entry = {
  name: string;
  qid: qid;
  mode: perm_int * dir_entry_mode;
  length: int64;
  
  atime: int;
  mtime: int;
 
  uid: string;
  gid: string;
  muid: string;

  _typ: int16;
  _dev: int;
}
  and perm_int = int
  and dir_entry_mode =
    | DMDir
    | DMFile

type open_flags = perm_property

(* conversions needed when calling the C code *)
val int_of_qid_type: qid_type -> int
val int_of_perm_property: perm_property -> int
val open_flags_of_int: int -> open_flags

(* similar to Unix.Unix_error but for Plan9 *)
exception Plan9_error of string (* cmd *) * string (* errstr *)

type namespace_flag =
  | MRepl
  | MBefore
  | MAfter


val bind: 
  < Cap.bind; .. > ->
  Fpath.t (* src *) -> Fpath.t (* dst *) -> namespace_flag -> unit

val mount: 
  < Cap.mount; .. > ->
  Unix.file_descr -> int (* ?? *) -> Fpath.t (* dst *) -> namespace_flag ->
  string (* args *) -> unit


val mk_dir_entry: string -> qid -> perm_int * dir_entry_mode -> dir_entry
