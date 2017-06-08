open Common
open Point
open Rectangle

open Window

module W = Window
module I = Display

type event = 
  (* reading from keyboard thread *)
  | Key   of Keyboard.key
  (* reading from mouse thread *)
  | Mouse of Mouse.state
  (* reading from many places *)
  | Cmd   of Window.cmd

  (* producing for thread_fileserver(Read Qmouse) *)
  | SentChannelForMouseRead

let debug = ref false
let cnt = ref 0


let key_in w key =
  (* less: if key = 0? *)
  if not w.deleted then begin

    if !debug then begin
      incr cnt;
      let len = Char.code key in
      let r = Rectangle.r 0 0 len 1 
        |> Rectangle.add_pt w.screenr.min 
        |> Rectangle.add_pt (Vector.v 0 !cnt)
      in
      Draw.draw w.img r !Globals.red None Point.zero;
    (*Text.string w.img r.min !Globals.red Point.zero !Globals.font;*)
    end;
    
    (* less: navigation keys *)
    (* todo: if rawing *)
    (* todo: if holding *)
    ()
  end

let mouse_in w m =
  if !debug then begin
    let r = Rectangle.r 0 0 1 1 
      |> Rectangle.add_pt m.Mouse.pos
      |> Rectangle.add_pt (Vector.v 10 10)
    in
    Draw.draw w.img r !Globals.red None Point.zero;
  end;
  w.last_mouse <- m;
  match w.mouse_opened with
  | true -> 
    w.mouse_counter <- w.mouse_counter + 1;
    (* less: limit queue length? *)
    if m.Mouse.buttons <> w.last_buttons 
    then begin 
      Queue.add (m, w.mouse_counter) w.mouseclicks_queue;
      w.last_buttons <- m.Mouse.buttons
    end;
  | false -> failwith "mouse_in: mouse not opened todo"

let mouse_out w chan =
  (*/* send a queued event or, if the queue is empty, the current state */
    /* if the queue has filled, we discard all the events it contained. */
    /* the intent is to discard frantic clicking by the user during long latencies. */
  *)
  let m, counter =
    if Queue.length w.mouseclicks_queue > 0
    then Queue.take w.mouseclicks_queue 
    else w.last_mouse, w.mouse_counter
  in
  w.last_count_sent <- counter;
  Event.send chan m |> Event.sync


let cmd_in w cmd =
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


let wrap f = 
  fun ev -> Event.wrap ev f

let thread w =
  
  (* less: threadsetname *)

  (* less: extra channel creation? *)
  let chan_devmouse = Event.new_channel () in

  while true do
    (* less: adjust event set *)
    let ev = (
    (* receive *)
    [ 
      Event.receive w.chan_keyboard |> wrap (fun x-> Key x);
      Event.receive w.chan_mouse    |> wrap (fun x-> Mouse x);
      Event.receive w.chan_cmd      |> wrap (fun x-> Cmd x);
    ] @
    (* sending *)
    if w.mouse_counter <> w.last_count_sent 
    then [Event.send w.chan_devmouse_read chan_devmouse 
           |> wrap (fun () -> SentChannelForMouseRead)]
    else []
    ) |> Event.select
    in
    (match ev with
    | Key key -> key_in w key
    | Mouse m -> mouse_in w m
    | Cmd cmd -> 
      (* todo: if return Exited then threadsexit and free channels.
       * When answer Exited? when Cmd is Exited?
       *)
      cmd_in w cmd
    | SentChannelForMouseRead -> mouse_out w chan_devmouse
    );
    if not w.deleted
    then Image.flush w.img;
  done
