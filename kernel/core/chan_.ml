open Types

type qid = {
  (* less: really need more than 32billions files? *)
  qpath: int64; 
  qver: int;
  qtype: qid_kind;
}
and qid_kind =
  | QFile
  | QDir


type open_mode =
  | ORead
  | OWrite
  | OReadWrite

type t = {
  qid: qid;
  chantype: devid;
  (* less: dev: int *)

  mode: open_mode;
  offset: int64;

  path: string list;
  ismtpt: bool;

  (* The spinlock in this ref is also Chan's lock *)
  refcnt: Ref_.t;
}
