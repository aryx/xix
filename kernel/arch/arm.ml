(* memory-mapped IO *)
(* less: raspberry-specific *)
let physIO = P (0x3F000000) (* raspi1 = 0x20000000 *)
let virtIO = V (0x3E000000, Kernel) (* TODO: should be 0x7E000000 *)

let arch = {
  x = 1;
}
