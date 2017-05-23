open Common
open Window

module I = Display

type event = 
  | Key   of Keyboard.key
  | Mouse of Mouse.t
  | Cmd   of Window.cmd

let key_control w key =
  (* less: if key = 0? *)
  if not w.deleted then begin
    (* less: navigation keys *)
    (* todo: if rawing *)
    (* todo: if holding *)
    ()
  end


let thread w =
  
  (* less: threadsetname *)
  let display = w.img.I.display in

  (* todo: channel creation *)
  while true do
    (* less: adjust event set *)
    let ev =
    [ 
      Event.receive w.chan_keyboard |> (fun ev->Event.wrap ev(fun x-> Key x));
      Event.receive w.chan_mouse    |> (fun ev->Event.wrap ev(fun x-> Mouse x));
    ] |> Event.select
    in
    (match ev with
    | Key key -> key_control w key
    | Mouse m -> 
      (match w.mouse_opened with
      | true -> raise Todo
      | false -> raise Todo
      )
    | Cmd cmd -> raise Todo
    );
    if not w.deleted
    then Display.flush display;
  done
