open Common

(* supported archs by xix toolchain *)
type t =
  | Arm
  | Arm64
  | Mips
  | Riscv
  | Riscv64
  | X86
  | Amd64

(* Plan 9 conventions *)
let thechar (x : t) : char =
  match x with
  | Arm -> '5'
  | Arm64 -> '7'
  | Mips -> 'v'
  | Riscv -> 'i'
  | Riscv64 -> 'j'
  | X86 -> '8'
  | Amd64 -> '6'

let thestring (x : t) : string =
  match x with
  | Arm -> "arm"
  | Arm64 -> "arm64"
  | Mips -> "mips"
  | Riscv -> "riscv"
  | Riscv64 -> "riscv64"
  | X86 -> "386"
  | Amd64 -> "amd64"

let arch_of_char (c : char) : t =
  match c with
  | '5' -> Arm 
  | '7' -> Arm64 
  | 'v' -> Mips 
  | 'i' -> Riscv 
  | 'j' -> Riscv64 
  | '8' -> X86 
  | '6' -> Amd64 
  | _ -> failwith (spf "unrecognized arch character %c" c)
