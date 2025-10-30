open Common
open Regexp_.Operators

open Point

(* todo: delete once threadUnix is not needed anymore *)
module Unix1 = Unix
module Unix2 = (*Thread*)Unix

module C = Cursor
module M = Draw_marshal

(*****************************************************************************)
(* Mouse state *)
(*****************************************************************************)

type state = {
  pos: Point.t;
  buttons: buttons;
  (* ?? *)
  msec: int;
}
  (* rio does not use the possibility to click on multiple buttons at the same
   *  time, but some applications might, so we need to provide the whole state.
   * less: opti: bitset
   *)
  and buttons = { left: bool; middle: bool; right: bool; }

let nobuttons = 
  { left = false; middle = false; right = false }

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

let mk pos button = 
  { pos = pos;
    buttons = 
      (match button with
      | Left -> { nobuttons with left = true }
      | Middle -> { nobuttons with middle = true }
      | Right -> { nobuttons with right = true }
      );
    msec = -1;
  }

let fake_state = {
  pos = Point.zero;
  buttons = { left = false; middle = false; right = false };
  msec = 0;
}  

(*****************************************************************************)
(* Parsers *)
(*****************************************************************************)
       
let buttons_of_int i =
  (* less: sanity check between 0 and 7 *)
  { left   = (i land 1) <> 0; 
    middle = (i land 2) <> 0; 
    right  = (i land 4) <> 0;
  }

let int_of_buttons buttons =
  (if buttons.left then 1 else 0) lor
  (if buttons.middle then 2 else 0) lor
  (if buttons.right then 4 else 0)


(*****************************************************************************)
(* Mouse device controller *)
(*****************************************************************************)

type ctl = {
  (* /dev/mouse *)
  fd: Unix1.file_descr;
  (* streams of mouse events that can be received from thread_mouse below *)
  chan: state Event.channel;

  (* /dev/cursor *)
  cursor_fd: Unix1.file_descr;
  (* todo: resize_chan: unit Event.channel; *) 

  (* having the state in the ctl is less functional that forcing the
   * programmer to each time take a ctl and a mouse, but it can be
   * tedious to pass and return around each time the last state. 
   * todo: but where to set it? before send? in read? in receive?
   *)
  (*mutable state: Mouse.t;*)
}

let thread_mouse ctl =
  (* less: threadsetname? *)
  (* 'm':1 [xpos:4] [ypos:4] [button:4] [mseg:4] *)
  let bufsize = 1 + 4*12 in
  let buf = Bytes.make bufsize ' ' in
  while true do
    let n = Unix2.read ctl.fd buf 0 bufsize in
    if n <> bufsize
    then failwith (spf "wrong format in /dev/mouse; read %d chars (%s)" 
                     n (String.escaped (Bytes.to_string buf)));

    let str_at n = 
      let s = Bytes.sub_string buf (1 + (n * 12)) 12 in
      if s =~ "^[ ]*\\([^ ]+\\)[ ]*$"
      then Regexp_.matched1 s
      else failwith (spf "not a /dev/mouse entry, got %s" s)
    in
    let int_at n = 
      try int_of_string (str_at n)
      with Failure _ -> failwith (spf "not an int at %d (%s)" n (str_at n))
    in
    
    (match Bytes.get buf 0 with
    | 'm' ->
      let m = {
        pos = { x = int_at 0; y = int_at 1; };
        buttons = buttons_of_int (int_at 2);
        msec = int_at 3;
      }
      in
      Event.send ctl.chan m |> Event.sync
    | 'r' -> failwith "Mouse.thread: resize event: TODO"
    | x -> failwith (spf "wrong format in /dev/mouse: %c (%s)" x 
                       (String.escaped (Bytes.to_string buf)))
    )
  done



(* less: take image parameter? *)
let init () =
  try 
   let (chan: state Event.channel) = Event.new_channel () in
   let fd        = Unix1.openfile "/dev/mouse"  [Unix1.O_RDWR] 0o666 in
   let cursor_fd = Unix1.openfile "/dev/cursor" [Unix1.O_RDWR]   0o666 in

   let ctl = 
    { fd = fd; chan = chan; cursor_fd = cursor_fd; (*state = fake_state*) } in

   let _thread = Thread.create thread_mouse ctl in
   ctl
  with Unix.Unix_error (err, cmd, arg) ->
    failwith (spf "Mouse.init: unix error '%s' while executing '%s' with '%s'"
      (Unix.error_message err) cmd arg)


let receive ctl =
  Event.receive ctl.chan

let read ctl =
  let m = receive ctl |> Event.sync in
  (* ctl.state <- m; *)
  m

let flush_and_read display ctl =
  Display.flush display;
  read ctl

  

(* hence O_RDWR for /dev/mouse *)
let _move_to _ctl _pt =
  failwith "Mouse.move_to: TODO"

(*****************************************************************************)
(* Cursor *)
(*****************************************************************************)

let set_cursor ctl cursor =
  let str = 
    M.bp_point cursor.C.offset ^ 
    (* a little bit inefficient probably *)
    ([cursor.C.clr; cursor.C.set] 
        |> Array.concat |> Array.to_list |> List.map (String.make 1)
        |> String.concat "")
  in
  (* less: sanity check ?*)
  Unix2.write ctl.cursor_fd (Bytes.of_string str) 0 (String.length str) |> ignore

let reset_cursor ctl =
  (* todo: this does not work because ocaml/unix/write.c call write
   * only when len > 0.
   *)
  (* Unix2.write ctl.cursor_fd "" 0 0 |> ignore *)
  set_cursor ctl Cursor.arrow
