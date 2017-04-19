(* arch-specific constants *)

(* todo: use cpp to get content of an arch_arm.ml? *)

(* todo: use Int32.t? *)
type int32 = int

type addr = int32
(* less: let wd2by = 4 *)

type ureg = {
  r0: int32;
  r1: int32;
  r2: int32;
  r3: int32;
  r4: int32;
  r5: int32;
  r6: int32;
  r7: int32;
  r8: int32;
  r9: int32;
  r10: int32;
  r11: int32;

  (* SB *)
  r12: int32;
  (* SP *)
  r13: int32;
  (* LINK *)
  r14: int32;
  (* PC *)
  r15: int32;

  psr: int32;
}

let pg2by = 0x1000 (* 4096 *)

(* todo: should be 0x80000000 but this big number cant be stored in OCaml int
 * todo: make sure mem.h agrees with that! and virt_io, etc.
*)
let kzero = 0x40000000 
let ktzero = kzero + 0x8000

(* less: let cpuaddr = V (kzero + 0x2000, Kernel) *)

(* _MAGIC(0, 20) where _MAGIC(f, b)	((f)|((((4*(b))+0)*(b))+7)) *)
let aout_magic = -1 (* todo:!! *)


(* less: need generic type?
type t = {
  pg2by: int;

  kzero:  ...
  ktzero: ...

  set_pc:
  set_sp: 
}
*)
