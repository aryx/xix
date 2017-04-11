open Types

type qid = {
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
  chantype: Device.devid;
  (* less: dev: int *)

  qid: qid;

  path: string list;

  offset: int64;

  mode: open_mode;
  
  ismtpt: bool;

  (* ref: Ref.t; *)
}
