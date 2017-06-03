open Common
open Point
open Rectangle

open Window

module W = Window
module I = Display

type event = 
  | Key   of Keyboard.key
  | Mouse of Mouse.state
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
    (*Text.string w.img r.min !Globals.red Point.zero !Globals.font;*)
    
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


let cmd_control w cmd =
  match cmd with
  | Delete -> 
    (* less: break if window already deleted *)
    (* todo: delete timeout process *)
    Wm.close_win w

  | Reshape (new_img, mouse) ->
    (* less: put all of that in Wm.resize_win ? *)
    if w.W.deleted
    (* less: free new_img if deleted, but when can happen? *)
    then failwith "window already deleted";
    let r = new_img.I.r in
    w.W.screenr <- r;
    Wm.resize_win w new_img;
    (* less: set wctlready to true *)
    (* todo: delete timeout proc for old name of window *)
    (match Rectangle.dx r, Globals.win () with
    | 0, Some w2 when w2 == w ->
      Wm.set_current_and_repaint_borders None mouse
    | n, Some w2 when (w2 == w) -> 
      (* less: could Wm.set_current_and_repaint_borders (Some w) mouse,
       * useless opti I think to special case here w2 == w
       *)
      ()
    | n, (Some _ | None) ->
      Wm.set_current_and_repaint_borders (Some w) mouse
    );
    (* less: Image.flush new_img, but useless cos done in thread () *)
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
      Event.receive w.chan_cmd      |> (fun ev->Event.wrap ev(fun x-> Cmd x));
    ] |> Event.select
    in
    (match ev with
    | Key key -> key_control w key
    | Mouse m -> mouse_control w m
    | Cmd cmd -> 
      (* todo: if return Exited then threadsexit and free channels.
       * When answer Exited? when Cmd is Exited?
       *)
      cmd_control w cmd
    );
    if not w.deleted
    then Image.flush w.img;
  done
