open Common
open Point
open Rectangle

open Window

module I = Display

type event = 
  | Key   of Keyboard.key
  | Mouse of Mouse.t
  | Cmd   of Window.cmd

let cnt = ref 0

let key_control w key =
  (* less: if key = 0? *)
  if not w.deleted then begin

    (* TODO: remove, just for debug *)
    incr cnt;
    let len = Char.code key in
    let r = Rectangle.r 0 0 len 1 
      |> Rectangle.add_pt w.screenr.min 
      |> Rectangle.add_pt (Vector.v 0 !cnt)
    in
    Draw.draw w.img r !Globals.red None Point.zero;
    
    (* less: navigation keys *)
    (* todo: if rawing *)
    (* todo: if holding *)
    ()
  end

let mouse_control w m =
(* TODO: remove, just for debug *)
  let r = Rectangle.r 0 0 1 1 
  |> Rectangle.add_pt m.Mouse.pos
  |> Rectangle.add_pt (Vector.v 10 10)
  in
  Draw.draw w.img r !Globals.red None Point.zero;
  (*
  match w.mouse_opened with
  | true -> raise Todo
  | false -> raise Todo
  *)
  ()


let thread w =
  
  (* less: threadsetname *)

  (* less: extra channel creation? *)
  while true do
    (* less: adjust event set *)
    let ev =
    [ 
      Event.receive w.chan_keyboard |> (fun ev->Event.wrap ev(fun x-> Key x));
      Event.receive w.chan_mouse    |> (fun ev->Event.wrap ev(fun x-> Mouse x));
      Event.receive w.chan_cmd      |> (fun ev->Event.wrap ev(fun x -> Cmd x));
    ] |> Event.select
    in
    (match ev with
    | Key key -> key_control w key
    | Mouse m -> mouse_control w m
    | Cmd cmd -> raise Todo
    );
    if not w.deleted
    then Image.flush w.img;
  done
