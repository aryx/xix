open Common

open Device
open Point

let dev_mouse = { (*Device.default with*)
  name = "mouse";
  perm = Plan9.rw;

  open_ = (fun (w : Window.t) ->
    if w.mouse_opened
    then raise (Error "file in use");
    w.mouse_opened <- true;
    (* less: resized <- false? and race comment? *)
    ()
  );
  close = (fun (w : Window.t) ->
    (* stricter? check that was opened indeed? *)
    w.mouse_opened <- false;
    (* todo: resized? Refresh message?*)
    ()
  );
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
  write_threaded = (fun _offset _str _w ->
    failwith "TODO: virtual_mouse.write_threaded"
  );
}

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
