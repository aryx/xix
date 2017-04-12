open Types

(* copy from device.ml to avoid mutual deps *)
type devid = int

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
  qid: qid;
  chantype: devid;
  (* less: dev: int *)

  mode: open_mode;
  offset: int64;

  path: string list;
  ismtpt: bool;

  (* ref: Ref.t; *)
}
