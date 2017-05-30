
(* less: use convert_flag_list strategy used in unix.ml? *)
let mREPL = 0x0000
let mBEFORE = 0x0001
let mAFTER = 0x0002

external bind: string -> string -> int -> int = "plan9_bind"
