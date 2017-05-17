open Common

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix

type key = char

type ctl = {
  chan: key (* less: buffer 20? *) Event.channel;
  (* /dev/cons *)
  fd: Unix1.file_descr;
  (* /dev/consctl *)
  consctl: Unix1.file_descr;
}

let thread_keyboard ctl =
  (* less: threadsetname *)
  let bufsize = 20 in
  let buf = String.make bufsize ' ' in
  while true do
    let n = Unix2.read ctl.fd buf 0 bufsize in
    if n <= 0
    then failwith (spf "wrong format in /dev/cons; read %d chars (%s)" 
                     n (String.escaped buf));
    (* todo: runes *)
    for i = 0 to n - 1 do
      (*pr (spf "sending %c" buf.[i]);*)
      let ev = Event.send ctl.chan buf.[i] in
      Event.sync ev
    done
  done
  
let init () =
  let (chan: key Event.channel) = Event.new_channel () in
  let fd = Unix1.openfile "/dev/cons" [Unix1.O_RDONLY] 0o666 in
  let consctl = Unix1.openfile "/dev/consctl" [Unix1.O_WRONLY] 0o666 in

  let ctl = { fd = fd; chan = chan; consctl = consctl } in

  let str = "rawon" in
  let n = Unix1.write consctl str 0 (String.length str) in
  if n <> String.length str
  then failwith ("Keyboard.init: can't turn on raw mode" );
  
  let thread = Thread.create thread_keyboard ctl in
  ctl

let receive ctl =
  Event.receive ctl.chan

    
