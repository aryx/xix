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

type addr = Arch.addr

type phys_addr = P of addr

(* there are a few places where functions accept either a user or kernel
 * address (e.g., Device_.t.read method)
 *)
type virt_addr = V of addr * space
   and space = User | Kernel

(* similar to virt_addr but more precise! better type signatures. *)
type user_addr = VU of addr
type kern_addr = VK of addr

(*****************************************************************************)
(* could be in proc.ml *)
(*****************************************************************************)
type pid = int

(*****************************************************************************)
(* could be in files.ml *)
(*****************************************************************************)

type fd = int

(* special symbols: '/', '..', '#' *)
type filename = string

(* todo? more precise type? ugo? rwx? *)
type perm = int

(*****************************************************************************)
(* could be in device.ml *)
(*****************************************************************************)
type devid = int

(*****************************************************************************)
(* could be in time.ml *)
(*****************************************************************************)
type sec = int

type time = unit (* TODO float? no! no float in the kernel! *)


(*****************************************************************************)
(* misc *)
(*****************************************************************************)
