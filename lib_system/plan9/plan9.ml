
(* less: use convert_flag_list strategy used in unix.ml? *)
let mREPL = 0x0000
let mBEFORE = 0x0001
let mAFTER = 0x0002

(* for errstr() *)
let errmax = 128

external bind: string -> string -> int -> int = 
  "plan9_bind"

external mount: Unix.file_descr -> int -> string -> int -> string -> int =
  "plan9_mount"

(* string must not be more than errmax, and you should set the first
 * char to '\000' if you want to reset the error string in the kernel
 * for this process.
 *)
external errstr: string -> int -> unit =
  "plan9_errstr"
