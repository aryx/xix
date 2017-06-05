open Common

type fid = Protocol_9P.fid

type qid_file =
  | Qroot

  | Qwinname

type qid_parsed_path = qid_file * Window.wid

(* simpler than Plan9.dir_entry *)
type dir_entry_short = {
  name: string;
  file: qid_file;
  typ: Plan9.qid_type;
  (* less: dir_entry_mode seems redundant with typ *)
  mode: Plan9.perm * Plan9.dir_entry_mode;
}

let entries = [
  { name = "."; file = Qroot; typ = Plan9.QTDir; mode = (0o500, Plan9.DMDir) };
]

(* fid server-side state *)
type t = {
  (* the key *)
  fid: fid;

  qid: Plan9.qid;
  entry: dir_entry_short;

  mutable opened: bool;
  mutable flag: Plan9.open_flag;

  (* less: could also use a wid *)
  w: Window.t;
}

let qid_parsed_path_of_qid qid =
  raise Todo

let alloc fid =
  raise Todo
