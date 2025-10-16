open Common

type qid = {
  path: int64;
  vers: int;
  typ: qid_type;
}
  and qid_type =
    | QTFile
    | QTDir

val int_of_qid_type: qid_type -> int

type perm_property = { r: bool; w: bool; x: bool }

val r: perm_property
val w: perm_property
val rw: perm_property
val rx: perm_property
val rwx: perm_property

val int_of_perm_property: perm_property -> int

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

val open_flags_of_int: int -> open_flags


type namespace_flag =
  | MRepl
  | MBefore
  | MAfter

val mount: Unix.file_descr -> int -> filename -> namespace_flag -> string ->
  unit

val bind: filename -> filename -> namespace_flag -> unit
