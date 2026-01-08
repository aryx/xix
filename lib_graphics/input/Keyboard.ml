(* Copyright 2015-2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = (*Thread*)Unix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Console keyboard (thread) API *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type key = Rune.t

type ctl = {
  chan: key (* less: buffer 20? *) Event.channel;
  (* /dev/cons *)
  fd: Unix1.file_descr;
  (* /dev/consctl *)
  consctl: Unix1.file_descr;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let thread_keyboard (ctl : ctl) =
  (* less: threadsetname *)
  let bufsize = 20 in
  let buf = Bytes.make bufsize ' ' in
  while true do
    let n = Unix2.read ctl.fd buf 0 bufsize in
    if n <= 0
    then failwith (spf "wrong format in /dev/cons; read %d chars (%s)" 
                     n (String.escaped (Bytes.to_string buf)));
    (* todo: rune parsing, so return runes instead of series of bytes (utf8) *)
    for i = 0 to n - 1 do
      (*pr (spf "sending %c" buf.[i]);*)
      Event.send ctl.chan (Bytes.get buf i) |> Event.sync
    done
  done

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
  
let init (caps : < Cap.keyboard; ..>) =
  let _ = caps#keyboard in
  let (chan: key Event.channel) = Event.new_channel () in
  let fd      = Unix1.openfile "/dev/cons"    [Unix1.O_RDONLY] 0o666 in
  let consctl = Unix1.openfile "/dev/consctl" [Unix1.O_WRONLY] 0o666 in

  let ctl = { fd = fd; chan = chan; consctl = consctl } in

  let str = Bytes.of_string "rawon" in
  let n = Unix1.write consctl str 0 (Bytes.length str) in
  if n <> Bytes.length str
  then failwith ("Keyboard.init: can't turn on raw mode" );
  
  let _thread = Thread.create thread_keyboard ctl in
  ctl

let receive (ctl : ctl) : key Event.event =
  Event.receive ctl.chan
