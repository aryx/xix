open Common
open Device

module F = File
module W = Window

let dev = { Device.default with
  name = "cons";
  perm = Plan9.rw;

  (* a process is reading on its /dev/cons; we must read from key
   * events coming to the window, events sent from the keyboard thread.
   *)
  read_threaded = (fun offset count w ->
    (* less: flushtag *)
    (* less: handle unicode partial runes *)
    let (chan_count_out, chan_bytes_in) = 
      Event.receive w.W.chan_devcons_read |> Event.sync in
    Event.send chan_count_out count |> Event.sync;
    (* less: handle if flushing *)
    (* less: qlock active *)
    let bytes = Event.receive chan_bytes_in |> Event.sync in
    (* we asked for count so honor_count is redundant *)
    Device.honor_count count bytes
  );
}
