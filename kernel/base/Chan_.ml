open Common

type qid = {
  (* less: really need more than 32 billions files? *)
  qpath: int64; 
  qver: int;
  qtype: qid_kind;
}
and qid_kind =
  | QFile
  | QDir
  (* less: more stuff here*)


(* less: opti: use a bitset *)
type open_mode = {
  read: bool;
  write: bool;
  (* less: more stuff here*)
}

type t = {
  qid: qid;
  chantype: Types.devid;
  (* less: dev: int *)

  mode: open_mode;
  offset: int64;

  path: string list;
  ismtpt: bool;

  (* The spinlock in this ref is also Chan's lock *)
  refcnt: Ref_.t;
}
