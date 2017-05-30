
(* todo: type qid *)
(* todo: type qid_kind *)

(* todo: type dir_entry *)


(* M for mount, but also used for bind *)
type namespace_flag = 
  | MRepl
  | MBefore
  | MAfter

(* for errstr() *)
let errmax = 128

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
