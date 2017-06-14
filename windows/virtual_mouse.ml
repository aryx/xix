open Common
open Device

module F = File
module W = Window

let dev_mouse = { Device.default with
  name = "mouse";
  perm = Plan9.rw;
  open_ = (fun w ->
    if w.W.mouse_opened
    then raise (Error "file in use");
    w.W.mouse_opened <- true;
    (* less: resized <- false? and race comment? *)
    ()
  );
  close = (fun w ->
    (* less: stricter? check that was opened indeed? *)
    w.W.mouse_opened <- false;
    (* todo: resized? Refresh message?*)
    ()
  );
  (* a process is reading on its /dev/mouse; we must read from mouse
   * events coming to the window, events sent from the mouse thread.
   *)
  read_threaded = (fun _offset count w ->
    (* less: flushtag *)
    (* less: qlock active *)
    (* less: qlock unactive after answer? so need reorg this func? *)
    let chan = Event.receive w.W.chan_devmouse_read |> Event.sync in
    let m    = Event.receive chan |> Event.sync in
    (* less: resize message *)
    let str = 
      spf "%c%11d %11d %11d %11d " 
        'm' m.Mouse.pos.Point.x m.Mouse.pos.Point.y 
        (Mouse.int_of_buttons m.Mouse.buttons) m.Mouse.msec
    in
    (* bugfix: note that we do not honor_offset. /dev/mouse is a dynamic file *)
    Device.honor_count count str
  );
  write_threaded = (fun _offset str w ->
    failwith "TODO: virtual_mouse.write_threaded"
  );


}

let dev_cursor = { Device.default with
  name = "cursor";
  perm = Plan9.rw;
  open_ = (fun w ->
    raise Todo
  );
  close = (fun w ->
    raise Todo
  );
  read_threaded = (fun offset count w  ->
    raise Todo
  );
  write_threaded = (fun offset str w ->
    raise Todo
  );
}
