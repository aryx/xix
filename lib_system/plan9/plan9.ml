
(* todo: constructor to sanity check *)
type int16 = int
(* todo: use Int32.t *)
type int32 = int
(* todo: use Int64.t *)
type int64 = int

type qid = {
  path: int64;
  vers: int;
  (* less: opti: bitset *)
  typ: qid_type;
  (* less: extra flags? *)
}
and qid_type =
  | QTFile
  | QTDir
(* | QTMount | QTAuth | QTAppend | QTExcl *)

let int_of_qid_type = function
  | QTFile -> 0x00
  | QTDir -> 0x80

(* todo: better type? rwx? *)
type perm = int

(* old: called only Dir in libcore-C but this is wrong *)
type dir_entry = {
  (* file data *)
  name: string;
  qid: qid;
  mode: perm * dir_entry_mode;
  length: int64;
  
  atime: int;
  mtime: int;

  uid: string;
  gid: string;
  muid: string;

  (* system-modified data *)
  _typ: int16;
  _dev: int;
}
 (* hack because I don't have Int32 yet and DMDIR requires 32 bits,
  * but good to separate the perm from this information I think
  *)
  and dir_entry_mode =
    | DMDir (* 0x80000000 *)
    | DMFile

let mk_dir_entry name qid perm = 
  { name = name; qid = qid; mode = perm;
    length = 0;
    atime = 0; mtime = 0;
    uid = ""; gid = ""; muid = "";
    _typ = 0; _dev = 0;
  }


  

(* M for mount, but also used for bind *)
type namespace_flag = 
  | MRepl
  | MBefore
  | MAfter
  (* | MCreate | MCache | MOrder *)

(* less: reuse Unix.open_flag? *)
type open_flag = int
 (* ORead | OWrite | ORdwr | OExec | OTrunc | OCExec | ORClose | OExcl *)

(* for errstr() *)
let errmax = 128




exception Plan9_error of string (* cmd *) * string (* errstr *)

let _ = Callback.register_exception "Plan9.Plan9_error"
                                    (Plan9_error("", ""))


external plan9_bind: string -> string -> int -> int = 
  "plan9_bind"

(* less: use convert_flag_list strategy used in unix.ml? *)
let namespace_flag_to_int = function
  | MRepl -> 0x0000
  | MBefore -> 0x0001
  | MAfter -> 0x0002

(* less: flags? and a namespace_flags_to_int that fold lor? *)
let bind src dst flag =
  plan9_bind src dst (namespace_flag_to_int flag)

external plan9_mount: Unix.file_descr -> int -> string -> int -> string -> int =
  "plan9_mount"

let mount fd int1 dst flag args =
  plan9_mount fd int1 dst (namespace_flag_to_int flag) args

(* string must not be more than errmax, and you should set the first
 * char to '\000' if you want to reset the error string in the kernel
 * for this process.
 *)
external errstr: string -> int -> unit =
  "plan9_errstr"
