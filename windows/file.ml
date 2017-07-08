open Common

open Plan9 (* for the fields *)
module N = Plan9

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* This is maintained by the "client" (the kernel on behalf of a winshell) *)
type fid = Protocol_9P.fid

(* This is returned by the server (rio) to identify a file of the server *)
type filecode = 
  | Dir of dir
  | File of devid
  and dir = 
    (* '/' old: was called Qdir in rio-C *)
    | Root
    (* less: '/wsys/' *)
  (* '/xxx' *)
  and devid = 
    | WinName
    | Mouse
    | Cons
    | ConsCtl

    | WinId
    (* todo: Cursor ... *)

(* will generate a Qid.path *)
type fileid = filecode * Window.wid

(* simpler than Plan9.dir_entry *)
type dir_entry_short = { 
  name: string;
  code: filecode;
  type_: Plan9.qid_type;
  (* just for the user; group and other are 'noperm' *)
  perm: Plan9.perm_property;
}

let root_entry = 
  { name = "."; code = Dir Root; type_ = N.QTDir; perm =  N.rx }

(* fid server-side state (a file) *)
type t = {
  (* The fid is maintained by the "client" (the kernel on behalf of winshell).
   * It is the key used to access information about a file used by
   * the client (it's redundant in File.t because it is also the key in
   * Fileserver.t.fids)
   *)
  fid: fid;

  (* The qid is what is returned by the "server" to identify a file (or dir).
   * It is mutable because a fid can be 'walked' to point to another file
   * on the server.
   *)
  mutable qid: Plan9.qid;
  (* for stat (mutable for the same reason than qid above) *)
  mutable entry: dir_entry_short;

  mutable opened: Plan9.open_flags option;

  (* less: we could also use a wid *)
  w: Window.t;

  (* less: nrpart for runes *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let int_of_filecode = function
  | Dir Root -> 0
  | File WinName -> 1
  | File Mouse -> 2
  | File Cons -> 3
  | File ConsCtl -> 4
  | File WinId -> 5

let int_of_fileid (qxxx, wid) = 
  (wid lsl 8) lor
  int_of_filecode qxxx

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let qid_of_fileid file_id typ =
  { path = int_of_fileid file_id;
    typ = typ;
    vers = 0;
  }
