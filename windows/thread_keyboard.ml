open Common
open Keyboard

module W = Window

let thread kbd =
  (* less: threadsetname *)

  while true do
    let key = Keyboard.receive kbd |> Event.sync in
    (* temporary *)
    if key = 'q' 
    then exit 0;

    Globals.win () |> Common.if_some (fun win ->
      Event.send win.W.chan_keyboard key |> Event.sync
    )
  done
