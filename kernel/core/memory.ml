open Common
open Types

(* less: arm! *)
let pg2by = 0x1000


(* general constants *)
let kzero = 0x40000000 (* TODO: should be 0x80000000 but cant be stored in int*)
let ktzero = V (kzero + 0x8000, Kernel)

let uzero = V (0x0, User)
let utzero = V (0x0 + pg2by, User)

(* less: let cpuaddr = V (kzero + 0x2000, Kernel) *)

(* memory-mapped IO *)

(* less: raspberry-specific *)
let physIO = P (0x3F000000) (* raspi1 = 0x20000000 *)
let virtIO = V (0x3E000000, Kernel) (* TODO: should be 0x7E000000 *)


let virt_to_phys x =
  raise Todo

let phys_to_virt x =
  raise Todo

let memclear va size =
  raise Todo
