type fid = int

type t = {
  fid: fid;

  mutable opened: bool;
  mutable mode: Unix.open_flag list;

  (* todo: qid: *)

  (* todo: w: Window.t; or use wid? *)
}

