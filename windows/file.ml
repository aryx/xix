open Common
open Plan9 (* for the fields *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type fid = Protocol_9P.fid

type filecode =
  (* old: was called Qdir in rio-C *)
  | Qroot

  | Qwinname

(* will generate Qid.path *)
type fileid = filecode * Window.wid

(* simpler than Plan9.dir_entry *)
type dir_entry_short = { 
  name: string;
  code: filecode;
  type_: Plan9.qid_type;
  (* just for the user, group and other are noperm *)
  perm: Plan9.perm_property;
}


let r  = { r = true; w = false; x = false }
let w  = { r = false; w = true; x = false }
let rw = { r = true; w = true; x = false }
let rx = { r = true; w = false; x = true }

let root_entry = 
  { name = "."; code = Qroot; type_ = Plan9.QTDir; perm =  rx }
let toplevel_entries = [
  { name = "winname"; code = Qwinname; type_ = Plan9.QTFile; perm = r };
]

(* fid server-side state (a file) *)
type t = {
  (* the fid is maintained by the "client" (the kernel on behalf of winshell) *)
  fid: fid;
  (* The qid is what is returned by the "server" to identify a file (or dir).
   * It is mutable because a fid can be 'walked' to point to another file
   * on the server.
   *)
  mutable qid: Plan9.qid;
  (* for stat, mutable for the same reason *)
  mutable entry: dir_entry_short;

  mutable opened: Plan9.open_flag option;

  (* less: we could also use a wid *)
  w: Window.t;

  (* less: nrpart for runes *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let int_of_filecode = function
  | Qroot -> 0
  | Qwinname -> 1

let int_of_fileid (qxxx, wid) = 
  (wid lsl 8) lor
  int_of_filecode qxxx

let qid_of_fileid file_id typ =
  { path = int_of_fileid file_id;
    typ = typ;
    vers = 0;
  }

let fileid_of_qid qid =
  raise Todo


let alloc fid =
  raise Todo
