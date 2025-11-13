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
 * to rely on because we also compile Xix with "ocaml light" (our own
 * fork of ocaml). Enter this file.
 *
 * alternatives:
 *  - use conditional compilation to use https://github.com/dbuenzli/logs,
 *    which is available as the "logs" OPAM package when possible, and this
 *    file otherwise. However, the original logs.ml is only 300 LOC
 *    and does not do much, and use functors and first-class modules
 *    which we actually don't want to support, so simpler to just reimplement
 *    a subset of it for now.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type level = App | Error | Warning | Info | Debug

(* The builtin compare will work on level because of the order in which
 * we defined the constructors. App < Error < Warning < ...
 * alt (Stdcompat.Stdlib.compare)
 *)
let compare_level = compare

(* orig: type ('a, 'b) msgf = ?header:... -> ?tags:... 
 * (('a, Format.formatter, unit, unit) format4 -> 'a) -> unit
 * but simpler to not use Format.formatter.
*)
type 'a msgf = (('a, out_channel, unit) format -> 'a) -> unit

(* The type of the logging functions (e.g., Logs.err) is ['a log].
 * A call usually looks like [Logs.err (fun m -> m "this is bad %s" err)]
 * meaning the first parameter is a function taking a 'm' which is
 * a "messaging function" (msgf) taking itself a format string and some
 * extra parameters in 'a and returning unit.
 *)
type 'a log = 'a msgf -> unit

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let current_level = ref (Some Warning)

let now () : float = Unix.gettimeofday ()

let time_program_start = now ()

(*****************************************************************************)
(* Reporter *)
(*****************************************************************************)

let header_string_of_level (lvl: level) : string =
  match lvl with
  | App -> ""
  | Error -> "ERROR"
  | Warning -> "WARNING"
  | Info -> "INFO"
  | Debug -> "DEBUG"

let color lvl str =
  let color =
    match lvl with
    | App -> Console.default
    | Error -> Console.red
    | Warning -> Console.yellow
    | Info -> Console.blue
    | Debug -> Console.green
  in
  Console.sprintf color "%s" str

let report (lvl : level) (msgf : 'a msgf) : unit =
  match lvl with
  | App -> 
      (* no header, no color *)
      msgf Printf.eprintf;
      (* %! is to flush the output *)
      Printf.eprintf "\n%!"
  | Error
  | Warning
  | Info ->
      Printf.eprintf "[%s]: " 
        (color lvl (header_string_of_level lvl));
      msgf Printf.eprintf;
      Printf.eprintf "\n%!"
  | Debug -> 
      let current = now () in
      (* KISS *)
      Printf.eprintf "[%05.2f][%s]: " 
        (current -. time_program_start)
        (color lvl (header_string_of_level lvl))
        ;
      msgf Printf.eprintf;
      Printf.eprintf "\n%!"

let msg (lvl : level) (msgf : 'a msgf) : unit =
  match !current_level with
  | None -> ()
  | Some current_level when compare_level lvl current_level > 0 ->
      ()
  | Some _ ->
      report lvl msgf

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* orig: takes a ?all:bool parameter too, to specify whether we should set
 * the level to the other srcs, but here we don't have any notion of "src" so
 * we don't need this extra parameter (and we don't want to use label arguments
 * as they are not supported by our "ocaml light" fork).
 *)
let set_level (lvlopt: level option) : unit =
  current_level := lvlopt

let app msgf = msg App msgf
let err msgf = msg Error msgf
let warn msgf = msg Warning msgf
let info msgf = msg Info msgf
let debug msgf = msg Debug msgf
