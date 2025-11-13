
(* supported OSes by xix toolchain *)
type t =
  (* the big ones *)
  | Linux
  | Windows
  | MacOS
  (* the educational ones *)
  | Plan9
  | Xv6

(* Note that many functions in the Sys module should work across OSes.
 * See also lib_system/{unix,plan9} os-specific libs.
 *)
