(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sys.ml,v 1.13 1997/09/11 15:10:23 doligez Exp $ *)

(* System interface *)

external get_config: unit -> string * int = "sys_get_config"
external get_argv: unit -> string array = "sys_get_argv"

let argv = get_argv()
let (os_type, word_size) = get_config()
let max_array_length = (1 lsl (word_size - 10)) - 1;;
let max_string_length = word_size / 8 * max_array_length - 1;;

external file_exists: string -> bool = "sys_file_exists"
external remove: string -> unit = "sys_remove"
external rename : string -> string -> unit = "sys_rename"
external getenv: string -> string = "sys_getenv"
external command: string -> int = "sys_system_command"
external chdir: string -> unit = "sys_chdir"
external getcwd: unit -> string = "sys_getcwd"

let interactive = ref false

type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)

external signal: int -> signal_behavior -> unit = "install_signal_handler"

(* ported from 3.12 *)
let set_signal sig_num sig_beh = ignore(signal sig_num sig_beh)


let sigabrt = -1
let sigalrm = -2
let sigfpe = -3
let sighup = -4
let sigill = -5
let sigint = -6
let sigkill = -7
let sigpipe = -8
let sigquit = -9
let sigsegv = -10
let sigterm = -11
let sigusr1 = -12
let sigusr2 = -13
let sigchld = -14
let sigcont = -15
let sigstop = -16
let sigtstp = -17
let sigttin = -18
let sigttou = -19
let sigvtalrm = -20
let sigprof = -21

exception Break

let catch_break on =
  if on then
    signal sigint (Signal_handle(fun _ -> raise Break))
  else
    signal sigint Signal_default
