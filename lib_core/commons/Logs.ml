(* Copyright 2024 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Poor's man logging library following the interface defined in
 * https://github.com/dbuenzli/logs by Daniel Bunzli.
 *
 * We need a logging library for Xix, but Logs from Bunzli can't be installed
 * on very old versions of OCaml like 3.10 that we want to support. Moreover,
 * it uses advanced features like functors and first-class modules
 * (as in `module Log = (val Logs.src_log src : Logs.LOG)`) that we don't want
 * to rely on because we plan to compile Xix with "ocaml light" (our own
 * fork of ocaml). Enter this file.
 *
 * alternatives:
 *  - use conditional compilation to use https://github.com/dbuenzli/logs,
 *    which is available as the "logs" OPAM package when possible, and this
 *    file otherwise. However, the original logs.ml is only 300 LOC
 *    and does not do much, and use functors and first-class modules
 *    which we actually don't want to support, so simpler to just reimplement
 *    a subset of it
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type level = App | Error | Warning | Info | Debug

(* orig: type ('a, 'b) msgf = ?header:... -> ?tags:... *)
type 'a msgf = (('a, Format.formatter, unit, unit) format4 -> 'a) -> unit

(* The type of the logging functions (e.g., Logs.err) is ['a log]
 * and a call usually looks like [Logs.err (fun m -> m "this is bad %s" err)]
 * meaning the first parameter is a function taking a 'm' which will is
 * a "messaging function" (msgf) taking a format string and some
 * extra parameters in 'a and returning unit.
 *)
type 'a log = 'a msgf -> unit

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* orig: takes a ?all:bool parameter too, to specify whether we should set
 * the level to the other srcs, but here we don't have any notion of "src" so
 * we don't need this extra parameter.
 *)
let set_level (lvlopt: level option) : unit =
  failwith "TODO"

let app msgf = failwith "TODO"
let err msgf = failwith "TODO"
let warn msgf = failwith "TODO"
let info msgf = failwith "TODO"
let debug msgf = failwith "TODO"
