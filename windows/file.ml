open Common
open Plan9 (* for the fields *)

type fid = Protocol_9P.fid

type filename =
  (* old: was called Qdir in rio-C *)
  | Qroot

  | Qwinname

(* will generate Qid.path *)
type file_id = filename * Window.wid

(* simpler than Plan9.dir_entry *)
type dir_entry_short = 
  string * (filename * Plan9.qid_type * Plan9.perm_property)

let r  = { r = true; w = false; x = false }
let w  = { r = false; w = true; x = false }
let rw = { r = true; w = true; x = false }
let rx = { r = true; w = false; x = true }

let root_entry = 
  ".", (Qroot, Plan9.QTDir, rx)
let entries = [
  "winname", (Qwinname, Plan9.QTFile, r)
]

(* fid server-side state (a file) *)
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

let path_of_qid qid =
  raise Todo

let alloc fid =
  raise Todo
