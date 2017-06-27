open Common
open Keyboard

module W = Window

let thread kbd =
  (* less: threadsetname *)

  while true do
    let key = Keyboard.receive kbd |> Event.sync in
    (* less: 
     *  - do that in other thread? so can start reading more keys? 
     *  - have sendp?
     *  - receive array of keys? nbrecv?
     *  - use double array of keys so can send and then receive without
     *    losing anything?
    *)
    Globals.win () |> Common.if_some (fun win ->
      Event.send win.W.chan_keyboard key |> Event.sync
    )
  done
