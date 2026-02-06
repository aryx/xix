(*s: Threads_window.ml *)
open Common

open Point
open Rectangle
open Window

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: type [[Threads_window.event]] *)
type event = 
  (* reading from keyboard thread *)
  | Key   of Keyboard.key
  (* reading from mouse thread *)
  | Mouse of Mouse.state
  (* reading from many places *)
  | Cmd   of Window.cmd

  (*s: [[Threads_window.event]] other cases *)
  (* producing for thread_fileserver(Read Qcons) *)
  | SentChannelsForConsRead
  (*x: [[Threads_window.event]] other cases *)
  (* producing and then consuming for thread_fileserver(Write Qcons) *)
  | SentChannelForConsWrite
  (*x: [[Threads_window.event]] other cases *)
  (* producing for thread_fileserver(Read Qmouse) *)
  | SentChannelForMouseRead
  (*e: [[Threads_window.event]] other cases *)
(*e: type [[Threads_window.event]] *)

(*****************************************************************************)
(* In and out helpers *)
(*****************************************************************************)
(*s: function [[Threads_window.key_in]] *)
(* input from user *)
let key_in (w : Window.t) (key : Keyboard.key) =
  (* less: if key = 0? when can happen? EOF? Ctrl-D? *)
  if not w.deleted then begin
    match w.raw_mode, w.mouse_opened with
    (*s: [[Threads_window.key_in()]] match [[raw_mode]] and [[mouse_opened]] cases *)
    (* todo: if holding *)
    | false, false -> 
      (* less: snarf *)
      Terminal.key_in w.terminal key
    (*x: [[Threads_window.key_in()]] match [[raw_mode]] and [[mouse_opened]] cases *)
    | true, true (* less: || q0 == nr *) ->
      Queue.add key w.raw_keys 
    (*x: [[Threads_window.key_in()]] match [[raw_mode]] and [[mouse_opened]] cases *)
    (* less: in theory we should allow also special navigation keys here *)
    | true, false  ->
      failwith "key_in: TODO: raw mode in textual window"
    (*x: [[Threads_window.key_in()]] match [[raw_mode]] and [[mouse_opened]] cases *)
    | false, true  ->
      failwith "key_in: TODO: buffered mode in graphical window"
    (*e: [[Threads_window.key_in()]] match [[raw_mode]] and [[mouse_opened]] cases *)
  end
(*e: function [[Threads_window.key_in]] *)
(*s: function [[Threads_window.runes_in]] *)
(* output from application *)
let runes_in (w: Window.t) (chan : Rune.t list Event.channel) =
  let runes = Event.receive chan |> Event.sync in
  Terminal.runes_in w.terminal runes
(*e: function [[Threads_window.runes_in]] *)

(*s: function [[Threads_window.mouse_in]] *)
let mouse_in (w : Window.t) (m : Mouse.state) =
  w.last_mouse <- m;
  match w.mouse_opened with
  | true -> 
    w.mouse_counter <- w.mouse_counter + 1;
    (* less: limit queue length? *)
    if m.buttons <> w.last_buttons 
    then begin 
      Queue.add (m, w.mouse_counter) w.mouseclicks_queue;
      w.last_buttons <- m.buttons
    end;

  | false -> failwith "mouse_in: mouse not opened todo"
(*e: function [[Threads_window.mouse_in]] *)
(*s: function [[Threads_window.mouse_out]] *)
let mouse_out (w : Window.t) (chan : Mouse.state Event.channel) =
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
(*e: function [[Threads_window.mouse_out]] *)

(*s: function [[Threads_window.bytes_out]] *)
let bytes_out (w : Window.t) (chan_count, chan_bytes) =
  let cnt = Event.receive chan_count |> Event.sync in
  let buf = Bytes.create cnt in
  let i = ref 0 in

  (match w.raw_mode with
  | true ->
    while !i < cnt && Queue.length w.raw_keys > 0 do
      Bytes.set buf !i (Queue.take w.raw_keys);
      incr i;
    done
  | false ->
    let term : Terminal.t = w.terminal in
    (* "When newline, chars between output point and newline are sent."*)
    while !i < cnt && term.output_point.i < term.nrunes do
      let pos = term.output_point.i in
      Bytes.set buf !i term.text.(pos);
      term.output_point <- { Terminal.i = pos + 1};
      incr i;
    done
  );

  let str =
    if !i < cnt
    then Bytes.sub_string buf 0 !i
    else Bytes.to_string buf
  in
  Event.send chan_bytes str |> Event.sync
(*e: function [[Threads_window.bytes_out]] *)

(*s: function [[Threads_window.cmd_in]] *)
let cmd_in (w : Window.t) (cmd : cmd) =
  match cmd with
  (*s: [[Threads_window.cmd_in()]] match [[cmd]] cases *)
  | Delete -> 
    (* less: break if window already deleted *)
    (* todo: delete timeout process *)
    Wm.close_win w
  (*x: [[Threads_window.cmd_in()]] match [[cmd]] cases *)
  | Reshape (new_img : Display.image) ->
    (* less: put all of that in Wm.resize_win ? *)

    if w.deleted
    (* less: free new_img if deleted, but when can happen? *)
    then failwith "window already deleted";

    let r = new_img.r in
    w.screenr <- r;
    Wm.resize_win w new_img;
    (* less: set wctlready to true *)
    (* todo: delete timeout proc for old name of window *)

    (match Rectangle.dx r, Globals.win () with
    | 0, Some w2 when w2 == w ->
      Wm.set_current_and_repaint None
    | _n, Some w2 when (w2 == w) -> 
      (* less: could Wm.set_current_and_repaint_borders (Some w) mouse,
       * useless opti I think to special case here w2 == w
       *)
      ()
    | _n, (Some _ | None) ->
      Wm.set_current_and_repaint (Some w)
    );
    (* less: Image.flush new_img, but useless cos done in thread () *)
    ()
  (*e: [[Threads_window.cmd_in()]] match [[cmd]] cases *)
(*e: function [[Threads_window.cmd_in]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[Threads_window.wrap]] *)
let wrap f = 
  fun ev -> Event.wrap ev f
(*e: function [[Threads_window.wrap]] *)

(*s: function [[Threads_window.thread]] *)
let thread (w : Window.t) =
  (* less: threadsetname *)
  (*s: [[Threads_window.thread()]] channels creation *)
  let chan_devcons_read_count = Event.new_channel () in
  let chan_devcons_read_bytes = Event.new_channel () in
  let chan_devcons_write_runes = Event.new_channel () in
  (*x: [[Threads_window.thread()]] channels creation *)
  let chan_devmouse = Event.new_channel () in
  (*e: [[Threads_window.thread()]] channels creation *)

  while true do
    let ev = (
    (* receive *)
    [ 
      Event.receive w.chan_keyboard |> wrap (fun x -> Key x);
      Event.receive w.chan_mouse    |> wrap (fun x -> Mouse x);
      Event.receive w.chan_cmd      |> wrap (fun x -> Cmd x);
    ] @
      (*s: [[Threads_window.thread()]] other [[select]] elements *)
      (* less: npart *)
      (if (w.raw_mode && Queue.length w.raw_keys > 0) ||
          (not w.raw_mode && Terminal.newline_after_output_point w.terminal)
       then [Event.send w.chan_devcons_read 
                (chan_devcons_read_count, chan_devcons_read_bytes)
              |> wrap (fun () -> SentChannelsForConsRead)]
       else []
      ) @
      (*x: [[Threads_window.thread()]] other [[select]] elements *)
      (* less: auto_scroll, mouseopen?? 
       * todo: qh vs org and nchars *)
      (if true
       then [Event.send w.chan_devcons_write chan_devcons_write_runes
            |> wrap (fun () -> SentChannelForConsWrite);]
       else []
      ) @
      (*x: [[Threads_window.thread()]] other [[select]] elements *)
      (* sending *)
      (if w.mouse_counter <> w.last_count_sent 
       then [Event.send w.chan_devmouse_read chan_devmouse 
              |> wrap (fun () -> SentChannelForMouseRead)]
       else []
      ) @
      (*e: [[Threads_window.thread()]] other [[select]] elements *)
      []
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
    (*s: [[Threads_window.thread()]] match [[ev]] other cases *)
    | SentChannelsForConsRead -> 
      bytes_out w (chan_devcons_read_count, chan_devcons_read_bytes)
    (*x: [[Threads_window.thread()]] match [[ev]] other cases *)
    | SentChannelForConsWrite ->
      runes_in w chan_devcons_write_runes
    (*x: [[Threads_window.thread()]] match [[ev]] other cases *)
    | SentChannelForMouseRead -> 
      mouse_out w chan_devmouse
    (*e: [[Threads_window.thread()]] match [[ev]] other cases *)
    );
    if not w.deleted
    then Image.flush w.img;
  done
(*e: function [[Threads_window.thread]] *)
(*e: Threads_window.ml *)
