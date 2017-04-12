(*****************************************************************************)
(* could be in common.ml *)
(*****************************************************************************)

type byte = char

type int8 = int
type int16 = int
(* todo: use Int32 *)
type int32 = int
(* todo: use Int64 *)
type int64 = int

(* todo: unicode *)
type rune = char

(*****************************************************************************)
(* could be in memory.ml *)
(*****************************************************************************)
type space = User | Kernel

(* todo: on 32 bits archi! *)
type addr = int32

type virt_addr = V of addr * space
type phys_addr = P of addr

(* dupe of virt_addr, but more precise in types *)
type user_addr = VU of addr
type kern_addr = VK of addr

(*****************************************************************************)
(* could be in time.ml *)
(*****************************************************************************)
type sec = int

type time = unit

(*****************************************************************************)
(* could be in files.ml *)
(*****************************************************************************)

type fd = int

(* special symbols: '/', '..', '#' *)
type filename = string

type perm = int

(*****************************************************************************)
(* misc *)
(*****************************************************************************)
