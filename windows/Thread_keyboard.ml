(*s: Thread_keyboard.ml *)
(* Copyright 2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reads from the keyboard and sends the key to the "current" window *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Thread_keyboard.thread]] *)
let thread (kbd : Keyboard.ctl) =
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
    Globals.win () |> Option.iter (fun (win : Window.t) ->
      Event.send win.chan_keyboard key |> Event.sync
    )
(*e: function [[Thread_keyboard.thread]] *)
  done
(*e: Thread_keyboard.ml *)
