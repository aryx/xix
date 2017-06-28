open Common

open Point
open Rectangle
open Window

module W = Window
module I = Display
module T = Terminal

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type event = 
  (* reading from keyboard thread *)
  | Key   of Keyboard.key
  (* reading from mouse thread *)
  | Mouse of Mouse.state
  (* reading from many places *)
  | Cmd   of Window.cmd

  (* producing for thread_fileserver(Read Qmouse) *)
  | SentChannelForMouseRead
  (* producing for thread_fileserver(Read Qcons) *)
  | SentChannelsForConsRead
  (* producing and then consuming for thread_fileserver(Write Qcons) *)
  | SentChannelForConsWrite

(*****************************************************************************)
(* In and out helpers *)
(*****************************************************************************)

(* input from user *)
let key_in w key =
  (* less: if key = 0? when can happen? EOF? Ctrl-D? *)
  if not w.deleted then begin
    match w.raw_mode, w.mouse_opened with
    | true, true (* less: || q0 == nr *) ->
      Queue.add key w.raw_keys 
    (* less: in theory we should allow also special navigation keys here *)
    | true, false  ->
      failwith "key_in: TODO: raw mode in textual window"
    | false, true  ->
      failwith "key_in: TODO: buffered mode in graphical window"
    (* todo: if holding *)
    | false, false -> 
      (* less: snarf *)
      Terminal.key_in w.W.terminal key
  end

(* output from application *)
let runes_in (w: Window.t) chan =
  let runes = Event.receive chan |> Event.sync in
  Terminal.runes_in w.W.terminal runes

let mouse_in w m =
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
  (* we use last_count_sent later to know if we are ready to send mouse
   * states to someone
   *)
  w.last_count_sent <- counter;
  Event.send chan m |> Event.sync


let bytes_out w (chan_count, chan_bytes) =
  let cnt = Event.receive chan_count |> Event.sync in
  let buf = String.create cnt in
  let i = ref 0 in

  (match w.raw_mode with
  | true ->
    while !i < cnt && Queue.length w.raw_keys > 0 do
      buf.[!i] <- Queue.take w.raw_keys;
      incr i;
    done
  | false ->
    let term = w.W.terminal in
    (* "When newline, chars between output point and newline are sent."*)
    while !i < cnt && term.T.output_point.T.i < term.T.nrunes do
      let pos = term.T.output_point.T.i in
      buf.[!i] <- term.T.text.(pos);
      term.T.output_point <- { T.i = pos + 1};
      incr i;
    done
  );

  let str =
    if !i < cnt
    then String.sub buf 0 !i
    else buf
  in
  Event.send chan_bytes str |> Event.sync



let cmd_in w cmd =
  match cmd with
  | Delete -> 
    (* less: break if window already deleted *)
    (* todo: delete timeout process *)
    Wm.close_win w

  | Reshape (new_img) ->
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
      Wm.set_current_and_repaint None
    | n, Some w2 when (w2 == w) -> 
      (* less: could Wm.set_current_and_repaint_borders (Some w) mouse,
       * useless opti I think to special case here w2 == w
       *)
      ()
    | n, (Some _ | None) ->
      Wm.set_current_and_repaint (Some w)
    );
    (* less: Image.flush new_img, but useless cos done in thread () *)
    ()


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let wrap f = 
  fun ev -> Event.wrap ev f

let thread w =
  
  (* less: threadsetname *)

  let chan_devmouse = Event.new_channel () in
  let chan_devcons_read_count = Event.new_channel () in
  let chan_devcons_read_bytes = Event.new_channel () in
  let chan_devcons_write_runes = Event.new_channel () in

  while true do
    let ev = (
    (* receive *)
    [ 
      Event.receive w.chan_keyboard |> wrap (fun x -> Key x);
      Event.receive w.chan_mouse    |> wrap (fun x -> Mouse x);
      Event.receive w.chan_cmd      |> wrap (fun x -> Cmd x);
    ] @
      (* sending *)
      (if w.mouse_counter <> w.last_count_sent 
       then [Event.send w.chan_devmouse_read chan_devmouse 
              |> wrap (fun () -> SentChannelForMouseRead)]
       else []
      ) @
      (* less: npart *)
      (if (w.raw_mode && Queue.length w.raw_keys > 0) ||
          (not w.raw_mode && Terminal.newline_after_output_point w.W.terminal)
       then [Event.send w.chan_devcons_read 
                (chan_devcons_read_count, chan_devcons_read_bytes)
              |> wrap (fun () -> SentChannelsForConsRead)]
       else []
      ) @
      (* less: auto_scroll, mouseopen?? 
       * todo: qh vs org and nchars *)
      (if true
       then [Event.send w.chan_devcons_write chan_devcons_write_runes
            |> wrap (fun () -> SentChannelForConsWrite);]
       else []
      )
    ) |> Event.select
    in
    (match ev with
    | Key key -> 
      key_in w key
    | Mouse m -> 
      mouse_in w m
    | Cmd cmd -> 
      (* todo: if return Exited then threadsexit and free channels.
       * When answer Exited? when Cmd is Exited?
       *)
      cmd_in w cmd
    | SentChannelForMouseRead -> 
      mouse_out w chan_devmouse
    | SentChannelsForConsRead -> 
      bytes_out w (chan_devcons_read_count, chan_devcons_read_bytes)
    | SentChannelForConsWrite ->
      runes_in w chan_devcons_write_runes
    );
    if not w.deleted
    then Image.flush w.img;
  done
