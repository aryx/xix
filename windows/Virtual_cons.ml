open Common
open Device

let dev_cons = { Device.default with
  name = "cons";
  perm = Plan9.rw;

  (* a process is reading on its /dev/cons; we must read from key
   * events coming to the window (events sent from the keyboard thread).
   *)
  read_threaded = (fun _offset count (w : Window.t) ->
    (* less: flushtag *)
    (* less: handle unicode partial runes *)
    let (chan_count_out, chan_bytes_in) = 
      Event.receive w.chan_devcons_read |> Event.sync in
    Event.send chan_count_out count |> Event.sync;
    (* less: handle if flushing *)
    (* less: qlock active *)
    let bytes = Event.receive chan_bytes_in |> Event.sync in
    (* we asked for count so honor_count is redundant below (but defensive) *)
    Device.honor_count count bytes
  );
  (* a process is writing on its /dev/cons; it wants to output strings
   * on the terminal
   *)
  write_threaded = (fun _offset str (w : Window.t) ->
    (* todo: partial runes *)
    let runes = Rune.bytes_to_runes str in
    (* less: flushtag *)
    let chan_runes_out = Event.receive w.chan_devcons_write |> Event.sync in
    (* less: handle if flushing *)
    (* less: qlock active *)
    Event.send chan_runes_out runes |> Event.sync;
  );
}

let dev_consctl = { Device.default with
  name = "consctl";
  perm = Plan9.w;
  
  open_ = (fun (w : Window.t) ->
    if w.consctl_opened
    then raise (Error "file in use");
    w.consctl_opened <- true;
  );
  close = (fun (w : Window.t) ->
    (* less: if holding *)
    if w.raw_mode
    then begin
      w.raw_mode <- false;
      (* less: send RawOff? but nop in Threads_window anyway *)
    end;
    w.consctl_opened <- false;
  );
    
  write_threaded = (fun _offset str (w : Window.t) ->
    match str with
    | "rawon" -> 
      (* less: holding *)
      (* stricter: set to bool, not increment, so no support 
       *  for multiple rawon *)
      (* stricter? exn if already on? *)
      w.raw_mode <- true;
      (* less: send RawOn? *)
    | "rawoff" ->
      (* todo: send RawOff so terminal can process remaining queued raw keys *)
      w.raw_mode <- false;
    | "holdon" ->
      failwith ("TODO: holdon")
    | "holdoff" ->
      failwith ("TODO: holdoff")
    | _ -> 
      raise (Error (spf "unknown control message: %s" str));
  );
}
