open Common

module F = File
module W = Window

(* todo? could spread in different virtual_cons.ml, virtual_mouse.ml, etc. *)

(* to be catched in thread_fileserver.ml and transformed in an Error 9P msg *)
exception Error of string

let dispatch_open file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in
  match filecode with
  | F.Qroot
  | F.Qwinname 
  | F.Qcons
    -> ()
  | F.Qmouse -> 
    if w.W.mouse_opened
    then raise (Error "file in use");
    w.W.mouse_opened <- true;
    (* less: resized <- false? and race comment? *)
    ()

let dispatch_close file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in
  match filecode with
  | F.Qroot
  | F.Qwinname 
  | F.Qcons
    -> ()
  | F.Qmouse -> 
    (* less: stricter? check that was opened indeed? *)
    w.W.mouse_opened <- false;
    (* todo: resized? Refresh message?*)
    ()

(* Executed in a thread because can wait for data on a channel.
 * Note that takes the offset because each device can honor or not
 * the offset requirements. For instance /dev/mouse does not but
 * /dev/winname does.
*)
let threaded_dispatch_read offset count file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in

  let honor_offset_and_count data =
    let len = String.length data in
    match () with
    | _ when offset > len -> ""
    | _ when offset + count > len ->
      String.sub data offset (len - offset)
    | _ -> data
  in
  let honor_count data =
    let len = String.length data in
    if len <= count
    then data
    else String.sub data 0 count
  in

  match filecode with
  | F.Qroot -> raise (Impossible "directories are handled in caller dispatch()")
  | F.Qwinname -> 
    let str = w.W.winname in
    let str = 
      if str = ""
      then raise (Error "window has no name")
      else str
    in
    honor_offset_and_count str

  (* a process is reading on its /dev/mouse; we must read from mouse
   * events coming to the window, events sent from the mouse thread.
   *)
  | F.Qmouse ->
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
    honor_count str

  (* a process is reading on its /dev/cons; we must read from key
   * events coming to the window, events sent from the keyboard thread.
   *)
  | F.Qcons ->
    (* less: flushtag *)
    (* less: handle unicode partial runes *)
    let (chan_count_out, chan_bytes_in) = 
      Event.receive w.W.chan_devcons_read |> Event.sync in
    Event.send chan_count_out count |> Event.sync;
    (* less: handle if flushing *)
    (* less: qlock active *)
    let bytes = Event.receive chan_bytes_in |> Event.sync in
    (* we asked for count so honor_count is redundant *)
    honor_count bytes
