open Common
(* Arch-specific constants and types.
 *
 * todo: this file should be generated from an arch_arm.ml or arch_x86.ml file.
 *)

type addr = int32
   (* todo: use Int32.t? *)
   and int32 = int

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
(* less: some ureg accessor: Ureg.pc, Ureg.sp, Ureg.set_pc? *)

let pg2by = 0x1000 (* 4096 *)

(* todo: should be 0x80000000 but this big number cant be stored in OCaml int
 * todo: make sure mem.h agrees with that! and virt_io, etc.
*)
let kzero = 0x40000000 
let ktzero = kzero + 0x8000 (* that's where the Raspberry Pi's ARM boots us *)

(* less: let cpuaddr = V (kzero + 0x2000, Kernel) *)

(* _MAGIC(0, 20) where _MAGIC(f, b)	((f)|((((4*(b))+0)*(b))+7)) *)
let aout_magic = -1 (* todo:!! *)

(* ticks frequency TODO: 1 for now, but should be 100 *)
let hz = 1

(* todo: for now; should be 4 for Raspi2 (anyway ocaml light is not multicore)*)
let max_cpus = 1

type t_fastticks = int64
   (* todo: use Int32.t? *)
   and int64 = int

(* fastticks frequency *)
let system_timer_hz = 62500000

let fastticks_frequency () =
  system_timer_hz
  
let fastticks () =
  (* read timer counter *)
  raise Todo

let timerset next =
  let now = fastticks () in
  let _period = next - now in
  (* less: sanitize period *)
  (* set timer counter *)
  let _ = raise Todo in
  ()

let us () =
  raise Todo

(* less:
let delay ms =
   raise Todo
let microdelay us =
   raise Todo
*)

(* less: need generic type?
type t = {
  pg2by: int;

  kzero:  ...
  ktzero: ...

  set_pc:
  set_sp: 
}
*)
