open Common
open Types

type addr = Types.addr
type phys_addr = Types.phys_addr
type user_addr = Types.user_addr
type kern_addr = Types.kern_addr

(* a few constants *)
let kb = 1024
let mb = 1024 * 1024 (* 1048576 *)
let gb = 1024 * 1024 * 1024 (* 1073741824 *)

let pg2by = Arch.pg2by

let roundup_page (VU addr) = 
  VU (Common.roundup addr pg2by)


(* general constants *)
let kzero = VK Arch.kzero
let ktzero = VK Arch.ktzero

let uzero = VU 0x0
let utzero = VU (0x0 + pg2by)

let virt_to_phys x =
  raise Todo

let phys_to_virt x =
  raise Todo
