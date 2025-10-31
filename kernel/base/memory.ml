open Common
open Types

type addr = Types.addr (* itself an alias for Arch.addr *)
type phys_addr = Types.phys_addr
type user_addr = Types.user_addr
type kern_addr = Types.kern_addr

(* a few constants *)
let kb = 1024
let mb = 1024 * 1024 (* 1048576 *)
let gb = 1024 * 1024 * 1024 (* 1073741824 *)

let pg2by = Arch.pg2by
let pgshift = Common2.log2 pg2by

(* general constants *)
let kzero = VK Arch.kzero
let ktzero = VK Arch.ktzero

let uzero = VU 0x0
let utzero = VU (0x0 + pg2by)

let ustktop = VU 0x2000000
let ustksize = 8 * mb

(* mv in arch.ml? *)
let virt_to_phys _x =
  raise Todo

let phys_to_virt _x =
  raise Todo
