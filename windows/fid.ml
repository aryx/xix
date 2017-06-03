open Common

type fid = Protocol_9P.fid

(* fid server-side state *)
type t = {
  fid: fid;

  qid: Plan9.qid;

  mutable opened: bool;
  mutable flag: Plan9.open_flag;

  (* less: could also use a wid *)
  w: Window.t;
}


let alloc fid =
  raise Todo
