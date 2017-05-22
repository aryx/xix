open Common
open Point

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = ThreadUnix

module C = Cursor
module M = Draw_marshal

type t = {
  pos: Point.t;
  buttons: buttons;
  (* ?? *)
  msec: int;
}

(* rio does not use the property that you can click multiple buttons 
 * at the same time, but some applications might, so we need to provide 
 * the whole state.
 * less: opti: bitset
 *)
and buttons = { left: bool; middle: bool; right: bool; }

(* this type is sometimes more convenient to use *)
type button = Left | Middle | Right

let has_click m = 
  let buttons = m.buttons in
  buttons.left || buttons.middle || buttons.right

let has_button m button =
  let buttons = m.buttons in
  match button with
  | Left -> buttons.left
  | Middle -> buttons.middle
  | Right -> buttons.right
  

type ctl = {
  (* /dev/mouse *)
  fd: Unix1.file_descr;
  (* streams of mouse events that can be received from mouse thread *)
  chan: t Event.channel;
  (* todo: resize_chan: unit Event.channel; *) 

  (* /dev/cursor *)
  cursor_fd: Unix1.file_descr;
}

let thread_mouse ctl =
  (* less: threadsetname? *)
  (* 'm':1 [xpos:4] [ypos:4] [button:4] [mseg:4] *)
  let bufsize = 1 + 4*12 in
  let buf = String.make bufsize ' ' in
  while true do
    let n = Unix2.read ctl.fd buf 0 bufsize in
    if n <> bufsize
    then failwith (spf "wrong format in /dev/mouse; read %d chars (%s)" 
                     n (String.escaped buf));

    let str_at n = 
      let s = String.sub buf (1 + (n * 12)) 12 in
      if s =~ "^[ ]*\\([^ ]+\\)[ ]*$"
      then Common.matched1 s
      else failwith (spf "not a /dev/mouse entry, got %s" s)
    in
    let int_at n = 
      try int_of_string (str_at n)
      with Failure _ -> failwith (spf "not an int at %d (%s)" n (str_at n))
    in

    
    (match buf.[0] with
    | 'm' ->
      let m = {
        pos = { x = int_at 0; y = int_at 1; };
        buttons = 
          (let i = int_at 2 in
           (* less: sanity check between 0 and 7 *)
           { left   = (i land 1) <> 0; 
             middle = (i land 2) <> 0; 
             right  = (i land 4) <> 0;
           });
        msec = int_at 3;
      }
      in
      Event.send ctl.chan m |> Event.sync
    | 'r' -> failwith "Mouse.thread: resize event: Todo"
    | x -> failwith (spf "wrong format in /dev/mouse: %c (%s)" x 
                       (String.escaped buf))
    )
  done

  

(* less: take image parameter? *)
let init () =
  let (chan: t Event.channel) = Event.new_channel () in
  let fd        = Unix1.openfile "/dev/mouse"  [Unix1.O_RDWR] 0o666 in
  let cursor_fd = Unix1.openfile "/dev/cursor" [Unix1.O_RDWR]   0o666 in

  let ctl = { fd = fd; chan = chan; cursor_fd = cursor_fd } in

  let thread = Thread.create thread_mouse ctl in
  ctl

let receive ctl =
  Event.receive ctl.chan

let flush_and_read display ctl =
  Display.flush display;
  receive ctl |> Event.sync

let read ctl =
  receive ctl |> Event.sync
  

(* hence O_RDWR for /dev/mouse *)
let move_to ctl pt =
  raise Todo

let set_cursor ctl cursor =
  let str = 
    M.bp_point cursor.C.offset ^ 
    (* a little bit inefficient probably *)
    ([cursor.C.clr; cursor.C.set] 
        |> Array.concat |> Array.to_list |> List.map (String.make 1)
        |> String.concat "")
  in
  Unix2.write ctl.cursor_fd str 0 (String.length str)
