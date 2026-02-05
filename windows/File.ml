(*s: File.ml *)
(* Copyright 2017-2027 Yoann Padioleau, see copyright.txt *)
open Common

open Plan9 (* for the fields *)
module N = Plan9

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ?? *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*s: type [[File.fid]] *)
(* This is maintained by the "client" (the kernel on behalf of a winshell) *)
type fid = Protocol_9P.fid
(*e: type [[File.fid]] *)

(*s: type [[File.filecode]] *)
(* This is returned by the server (rio) to identify a file of the server *)
type filecode = 
  | Dir of dir
  | File of devid
(*e: type [[File.filecode]] *)
(*s: type [[File.dir]] *)
  and dir = 
    (* '/' old: was called Qdir in rio-C *)
    | Root
(*e: type [[File.dir]] *)
    (* less: '/wsys/' *)
  (* '/xxx' *)
(*s: type [[File.devid]] *)
  and devid = 
    | WinName
    | Mouse
    (* todo: Cursor ... *)
    | Cons
    | ConsCtl

    | WinId
    | Text
(*e: type [[File.devid]] *)

(*s: type [[File.fileid]] *)
(* will generate a Qid.path *)
type fileid = filecode * Window.wid
(*e: type [[File.fileid]] *)

(*s: type [[File.dir_entry_short]] *)
(* simpler than Plan9.dir_entry *)
type dir_entry_short = { 
  name: string;
  code: filecode;
  type_: Plan9.qid_type;
  (* just for the user; group and other are 'noperm' *)
  perm: Plan9.perm_property;
}
(*e: type [[File.dir_entry_short]] *)

(*s: constant [[File.root_entry]] *)
let root_entry = 
  { name = "."; code = Dir Root; type_ = N.QTDir; perm =  N.rx }
(*e: constant [[File.root_entry]] *)

(*s: type [[File.t]] *)
(* fid server-side state (a file) *)
type t = {
  (* The fid is maintained by the "client" (the kernel on behalf of winshell).
   * It is the key used to access information about a file used by
   * the client (it's redundant in File.t because it is also the key in
   * Fileserver.t.fids)
   *)
  fid: fid;

  (* the state *)
  mutable opened: Plan9.open_flags option;

  (*s: [[File.t]] other fields *)
  (* The qid is what is returned by the "server" to identify a file (or dir).
   * It is mutable because a fid can be 'walked' to point to another file
   * on the server.
   *)
  mutable qid: Plan9.qid;
  (* for stat (mutable for the same reason than qid above) *)
  mutable entry: dir_entry_short;

  (* less: we could also use a wid *)
  win: Window.t;

  (* less: nrpart for runes *)
  (*e: [[File.t]] other fields *)
}
(*e: type [[File.t]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[File.int_of_filecode]] *)
let int_of_filecode = function
  | Dir Root -> 0
  | File WinName -> 1
  | File Mouse -> 2
  | File Cons -> 3
  | File ConsCtl -> 4
  | File WinId -> 5
  | File Text -> 6
(*e: function [[File.int_of_filecode]] *)

(*s: function [[File.int_of_fileid]] *)
let int_of_fileid (qxxx, wid) = 
  (wid lsl 8) lor
  int_of_filecode qxxx
(*e: function [[File.int_of_fileid]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[File.qid_of_fileid]] *)
let qid_of_fileid file_id typ =
  { path = int_of_fileid file_id;
    typ = typ;
    vers = 0;
  }
(*e: function [[File.qid_of_fileid]] *)
(*e: File.ml *)
