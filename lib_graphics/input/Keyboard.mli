
type key = Rune.t

type ctl = {
  chan: key (* less: buffer 20? *) Event.channel;
  (* /dev/cons *)
  fd: Unix.file_descr;
  (* /dev/consctl *)
  consctl: Unix.file_descr;
}

(* will create a keyboard thread reading ctl.fd and sending keys on
 * ctl.chan
 *)
val init: unit -> ctl

val receive: ctl -> key Event.event
