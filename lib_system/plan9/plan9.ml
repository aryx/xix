open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port (partial) of the Plan9 API to OCaml.
 * This is similar to unix.ml, but for Plan9.
 *  
 * I just binded the necessary API for rio to work:
 * bind, mount, errstr, and a few types for protocol_9P.ml.
 * 
 * less: provide full API in syscall.h and libc.h that is not
 *  already provided by unix.ml
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* uniQue file identifier (fileserver entity) *)
type qid = {
  path: int64;
  vers: int;
  typ: qid_type;
  (* less: extra flags? *)
}
  (* less: opti: bitset *)
  and qid_type =
    | QTFile
    | QTDir
   (* less: | QTMount | QTAuth | QTAppend | QTExcl *)

(* read/write/execute (DMExec: 0x1 DMWrite: 0x2 DMRead: 0x4) *)
type perm_property = { r: bool; w: bool; x: bool }

(* user/group/other (0oUGO) *)
type perm = { u: perm_property; g: perm_property; o: perm_property }

(* old: was called only 'Dir' in libcore-C but misleading *)
type dir_entry = {
  (* file data *)
  name: string;
  qid: qid;
  (* todo: dir_entry_mode redundant with Qid.typ? *)
  mode: perm_int * dir_entry_mode;
  length: int64;
  
  atime: int;
  mtime: int;
 
  (* note that called uid/gid but really the id is the full user/group name *)
  uid: string;
  gid: string;
  (* user who last modified the file *)
  muid: string;

  (* system-modified data *)
  _typ: int16;
  _dev: int;
}
   (* less: use 'perm' at some point *)
   and perm_int = int
 (* hack because I don't have Int32 yet and DMDIR requires 32 bits,
  * but good to separate the perm from this information I think.
  * less: seems redundant with qid_type?
  *)
  and dir_entry_mode =
    | DMDir (* 0x80000000 *)
    | DMFile
    (* DMAppend *)

(* M for mount, but also used for bind *)
type namespace_flag = 
  | MRepl
  | MBefore
  | MAfter
  (* | MCreate | MCache | MOrder *)

(* less: reuse Unix.open_flag? *)
type open_flags = perm_property (* ORead | OWrite | ORdwr | OExec *)
 (* less: OTrunc | OCExec | ORClose | OExcl *)

(* for errstr() *)
let errmax = 128

exception Plan9_error of string (* cmd *) * string (* errstr *)

let _ = Callback.register_exception "Plan9.Plan9_error"
                                    (Plan9_error("", ""))

(*****************************************************************************)
(* Conversions and builders *)
(*****************************************************************************)

let mk_dir_entry name qid perm = 
  { name = name; qid = qid; mode = perm;
    length = 0;
    atime = 0; mtime = 0;
    uid = ""; gid = ""; muid = "";
    _typ = 0; _dev = 0;
  }

let int_of_qid_type = function
  | QTFile -> 0x00
  | QTDir -> 0x80

(* less: use convert_flag_list strategy used in unix.ml? *)
let int_of_namespace_flag = function
  | MRepl -> 0x0000
  | MBefore -> 0x0001
  | MAfter -> 0x0002

(* common properties *)
let r  = { r = true; w = false; x = false }
let w  = { r = false; w = true; x = false }
let rw = { r = true; w = true; x = false }
let rx = { r = true; w = false; x = true }
let rwx = { r = true; w = true; x = true }

let open_flags_of_int mode =
  match mode with
  (* OREAD *)
  | 0 -> r
  (* OWRITE *)
  | 1 -> w
  (* ORDWR *)
  | 2 -> rw
  (* OEXEC *)
  | 3 -> { r = false; w = false; x = true }
  | _ -> failwith (spf "mode not yet supported: %d" mode)

let int_of_perm_property perm =
  (if perm.r then 0x4 else 0) lor
  (if perm.w then 0x2 else 0) lor
  (if perm.x then 0x1 else 0) lor
  0
  

(*****************************************************************************)
(* FFI *)
(*****************************************************************************)

external plan9_bind: string -> string -> int -> int = 
  "plan9_bind"
external plan9_mount: Unix.file_descr -> int -> string -> int -> string -> int =
  "plan9_mount"
(* string must not be more than errmax, and you should set the first
 * char to '\000' if you want to reset the error string in the kernel
 * for this process.
 *)
external errstr: string -> int -> unit =
  "plan9_errstr"

(* less: flags? and a namespace_flags_to_int that fold lor? *)
let bind src dst flag =
  plan9_bind src dst (int_of_namespace_flag flag)

let mount fd int1 dst flag args =
  plan9_mount fd int1 dst (int_of_namespace_flag flag) args
