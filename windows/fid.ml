open Common

(* todo: same than ninep.fid? *)
type fid = Protocol_9P.fid

type t = {
  fid: fid;

  mutable opened: bool;
  mutable mode: Unix.open_flag list;

  (* todo: qid: *)

  (* todo: w: Window.t; or use wid? *)
}


let alloc fcall_fid =
  raise Todo
