open Common
open Point

(* todo: remove once ocaml light threads emulates Unix directly *)
module Unix2 = ThreadUnix

type t = {
  xy: Point.t;
  buttons: buttons;
  msec: int;
}
(* less: opti: bitset *)
and buttons = {
  left: bool;
  middle: bool;
  right: bool;
}  

let has_click m = 
  let buttons = m.buttons in
  buttons.left || buttons.middle || buttons.right

type ctl = {
  (* /dev/mouse *)
  fd: Unix.file_descr;
  (* streams of mouse events that can be received from mouse thread *)
  chan: t Event.channel;
  (* less: resize_chan: unit Event.channel; *) 
}

let thread_mouse mousectl =
  (* less: threadsetname? *)
  let bufsize = 1 + 4*12 in
  let buf = String.make bufsize ' ' in
  while true do
    let n = Unix2.read mousectl.fd buf 0 bufsize in
    if n <> bufsize
    then failwith (spf "wrong format in /dev/mouse; read %d chars (%s)" 
                     n (String.escaped buf));

    let str_at n = 
      let s = String.sub buf (1 + (n * 12)) 12 in
      if s =~ "^[ ]*\\([^ ]+\\)[ ]*$"
      then Common.matched1 s
      else failwith (spf "not a /dev/draw/new entry, got %s" s)
    in
    let int_at n = 
      try int_of_string (str_at n)
      with Failure _ -> failwith (spf "not an int at %d (%s)" n (str_at n))
    in

    
    (match buf.[0] with
    | 'm' ->
      let m = {
        xy = { x = int_at 0; y = int_at 1; };
        buttons = 
          (match int_at 2 with
          | 0 -> { left = false; middle = false; right = false }
          | 1 -> { left = true; middle = false; right = false }
          | 2 -> { left = false; middle = true; right = false }
          | 4 -> { left = false; middle = false; right = true }
          | x -> failwith (spf "Mouse.thread_mouse: not handled %d" x)
          );
        msec = int_at 3;
      }
      in
      let ev = Event.send mousectl.chan m in
      Event.sync ev
    | 'r' -> raise Todo
    | x -> failwith (spf "wrong format in /dev/mouse: %c (%s)" x 
                       (String.escaped buf))
    )
  done

  

(* less: image *)
let init () =
  let (chan: t Event.channel) = Event.new_channel () in
  let fd = Unix.openfile "/dev/mouse" [Unix.O_RDONLY] 0o666 in
  let mousectl = { fd = fd; chan = chan } in

  let thread = Thread.create thread_mouse mousectl in
  mousectl

let receive mousectl =
  Event.receive mousectl.chan

let move_to mousectl pt =
  raise Todo
