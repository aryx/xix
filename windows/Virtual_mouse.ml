(*s: Virtual_mouse.ml *)
open Common

open Device
open Point

(*s: constant [[Virtual_mouse.dev_mouse]] *)
let dev_mouse = { (*Device.default with*)
  name = "mouse";
  perm = Plan9.rw;

  (*s: method [[Virtual_mouse.dev_mouse.open_]] *)
  open_ = (fun (w : Window.t) ->
    if w.mouse_opened
    then raise (Error "file in use");

    w.mouse_opened <- true;

    (* less: resized <- false? and race comment? *)
  );
  (*e: method [[Virtual_mouse.dev_mouse.open_]] *)
  (*s: method [[Virtual_mouse.dev_mouse.close]] *)
  close = (fun (w : Window.t) ->
    (* stricter? check that was opened indeed? *)

    w.mouse_opened <- false;

    (* todo: resized? Refresh message?*)
  );
  (*e: method [[Virtual_mouse.dev_mouse.close]] *)
  (*s: method [[Virtual_mouse.dev_mouse.read_threaded]] *)
  (* a process is reading on its /dev/mouse; we must read from mouse
   * events coming to the window, events sent from the mouse thread.
   *)
  read_threaded = (fun _offset count (w : Window.t) ->
    (* less: flushtag *)
    (* less: qlock active *)
    (* less: qlock unactive after answer? so need reorg this func? *)

    let chan = Event.receive w.chan_devmouse_read |> Event.sync in
    let m : Mouse.state    = Event.receive chan |> Event.sync in

    (* less: resize message *)

    let str = 
      spf "%c%11d %11d %11d %11d " 
        'm' m.pos.x m.pos.y (Mouse.int_of_buttons m.buttons) m.msec
    in
    (* bugfix: note that we do not honor_offset. /dev/mouse is a dynamic file *)
    Device.honor_count count str
  );
  (*e: method [[Virtual_mouse.dev_mouse.read_threaded]] *)
  (*s: method [[Virtual_mouse.dev_mouse.write_threaded]] *)
  write_threaded = (fun _offset _str _w ->
    failwith "TODO: virtual_mouse.write_threaded"
  );
  (*e: method [[Virtual_mouse.dev_mouse.write_threaded]] *)
}
(*e: constant [[Virtual_mouse.dev_mouse]] *)

(*s: constant [[Virtual_mouse._dev_cursor]] *)
let _dev_cursor = { (*Device.default with*)
  name = "cursor";
  perm = Plan9.rw;

  open_ = (fun _w ->
    raise Todo
  );
  close = (fun _w ->
    raise Todo
  );
  read_threaded = (fun _offset _count _w  ->
    raise Todo
  );
  write_threaded = (fun _offset _str _w ->
    raise Todo
  );
}
(*e: constant [[Virtual_mouse._dev_cursor]] *)
(*e: Virtual_mouse.ml *)
