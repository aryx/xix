(*s: Wm.ml *)
(* Copyright 2017-2026 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Window Manager *)

(*****************************************************************************)
(* Cursors *)
(*****************************************************************************)

(* less? move those cursor functions in cursors.ml? *)

(*s: function [[Wm.window_cursor]] *)
(* less: a rio_cursor? with lastcursor opti? and force parameter? *)
let window_cursor (w : Window.t) (pt : Point.t) (mouse : Mouse.ctl) : unit =
  let cursoropt = 
    (* less: if img is nil? if screenr is 0? *)
    match Globals.window_at_point pt with
    | Some w2 when w2  == w -> w.mouse_cursor
    | _ -> None
  in
  (* less: if menuing? or use corner_cursor() so no need this global? *)
  (* less: if holding *)
  match cursoropt with
  | Some x -> Mouse.set_cursor mouse x
  | None -> Mouse.reset_cursor mouse
(*e: function [[Wm.window_cursor]] *)

(*s: function [[Wm._corner_cursor]] *)
let _corner_cursor (w : Window.t) (pt : Point.t) (mouse : Mouse.ctl) =
  if Window.pt_on_border pt w
  then Mouse.set_cursor mouse (Cursors.which_corner_cursor w.screenr pt)
(*e: function [[Wm._corner_cursor]] *)

(*s: function [[Wm.corner_cursor_or_window_cursor]] *)
let corner_cursor_or_window_cursor (w : Window.t) (pt : Point.t) (mouse : Mouse.ctl) =
  if Window.pt_on_border pt w
  then Mouse.set_cursor mouse (Cursors.which_corner_cursor w.screenr pt)
  else window_cursor w pt mouse
(*e: function [[Wm.corner_cursor_or_window_cursor]] *)

(*****************************************************************************)
(* Borders and content *)
(*****************************************************************************)
(*s: function [[Wm.draw_border]] *)
let draw_border (w : Window.t) (status : Window.border_status) =
  let img : Display.image = w.img in
  (* less: if holding? *)
  let color = 
    match status with
    | Window.Selected   -> !Globals.title_color
    | Window.Unselected -> !Globals.title_color_light
  in
  Polygon.border img img.r Window.window_border_size color Point.zero
(*e: function [[Wm.draw_border]] *)

(* repaint border, content for textual window, (and todo cursor?) *)
(*s: function [[Wm.repaint]] *)
(* old: was called wrepaint in rio-C *)
let repaint (w : Window.t) =
  let status = 
    match Globals.win () with
    | Some w2 when w2 == w -> Window.Selected
    | _ -> Window.Unselected
  in
  draw_border w status;
  (* less: wsetcursor again? *)
  w.terminal.Terminal.is_selected <- (status = Window.Selected);
  if not w.mouse_opened 
  then Terminal.repaint w.terminal 
(*e: function [[Wm.repaint]] *)

(*s: function [[Wm.set_current_and_repaint]] *)
(* old: was called wcurrent() in rio.
 * alt: this function also sets the window cursor in rio-C, but this
 * requires then to pass a mouse parameter, which in turn requires to
 * pass the mouse in Reshape, which then requires to pass the mouse
 * parameter to hide_win and show_win and other functions which is
 * not super elegant. This is why I prefer to not pass a mouse parameter
 * which means the user may have to move the mouse to see the cursor
 * correctly updated after certain wm operations. I think this
 * tradeoff is ok.
 *)
let set_current_and_repaint wopt (*mouse*) =
  (* less: if wkeyboard *)
  let old = !Globals.current in
  Globals.current := wopt;
  (match old, wopt with
  | Some w2, Some w when not (w2 == w) ->
    (* bugfix: was doing repaint w, hmm *)
    repaint w2
  | _ -> ()
  );
  wopt |> Option.iter (fun w ->
    repaint w;
    (* TODO: do that in caller? so no need pass mouse? *)
    (* window_cursor w ptTODO mouse;*)

    (* todo: wakeup? why? *)
    ()
  )
(*e: function [[Wm.set_current_and_repaint]] *)

(*****************************************************************************)
(* Wm *)
(*****************************************************************************)

(*s: global [[Wm.threads_window_thread_func]] *)
(* will be set to Threads_window.thread in CLI.ml *)
let threads_window_thread_func: (Window.t -> unit) ref = ref (fun _ ->
  failwith "threads_window_thread_func undefined"
)
(*e: global [[Wm.threads_window_thread_func]] *)

(*s: function [[Wm.new_win]] *)
(* New window! new process! new thread!
 *
 * less: hideit, pid (but 0, or if != 0 -> use another func), scrolling
 *
 * CLI.thread_main -> Thread_mouse.thread -> Thread_mouse.wm_menu -> <>
 *)
let new_win (caps: < Cap.fork; Cap.exec; Cap.chdir; ..>) (img : Image.t) (cmd : string) (argv : string array) (pwd_opt : Fpath.t option)
    (_mouse, fs, font) : unit =

  (* A new Window.t *)

  (* less: cpid channel?  *)
  (* less: scrollit *)
  let w : Window.t = Window.alloc img font in
  (* less: wscrdraw here? (instead of in alloc, ugly) and draw(cols[BACK])? *)
  (* less: incref? *)

  (* simpler: draw_border w Window.Selected;
   * but done already later in set_current_and_repaint_borders
   *)

  (* A new window thread *)

  Hashtbl.add Globals.windows w.id w;
  let _win_thread = Thread.create !threads_window_thread_func w in

  (* less: if not hideit *)
  set_current_and_repaint (Some w);
  Image.flush img;

  (* A new window process *)

  pwd_opt |> Option.iter (fun str -> w.pwd <- str);

  (* TODO: Thread.critical_section := true; *)
  Logs.warn (fun m -> m "TODO: Thread.critical_section");
  let res = CapUnix.fork caps () in
  (match res with
  | -1 -> failwith "fork returned -1"
  | 0 ->
    (* child *)
    Processes_winshell.run_cmd_in_window_in_child_of_fork caps cmd argv w fs
  | pid -> 
    (* parent *)
    (* TODO: Thread.critical_section := false; *)
    w.pid <- pid;

    (* todo: how know if pb in child that require us then from
     * delete the window? need a cpid!
     *)

    (* old: was in wsetpid() *)
    w.label <- spf "rc %d" pid;
    (* less: notefd *)

    (* less: not too late? race with child to access /dev/winname? *)
    let winname = spf "window.%d" w.id in
    w.winname <- winname;
    Draw_ipc.name_image w.img winname;
    (* less: namecount and retry again if already used *)
  )
(*e: function [[Wm.new_win]] *)

(*s: function [[Wm.close_win]] *)
let close_win (w : Window.t) =
  w.deleted <- true;
  Globals.win () |> Option.iter (fun w2 ->
    if w2 == w
    then Globals.current := None;
    (* less: window_cursor  ?*)
  );
  (* less: if wkeyboard *)
  Hashtbl.remove Globals.hidden w.id;
  Hashtbl.remove Globals.windows w.id;
  Layer.free w.img;
  w.img <- Image.fake_image;
  ()
(*e: function [[Wm.close_win]] *)

(*s: function [[Wm.top_win]] *)
let top_win (w : Window.t) =
  if w.topped = !Window.topped_counter
  then ()
  else begin
    Layer.put_to_top w.img;
    set_current_and_repaint (Some w);
    Image.flush w.img;

    incr Window.topped_counter;
    w.topped <- !Window.topped_counter;
  end
(*e: function [[Wm.top_win]] *)

(*s: function [[Wm.hide_win]] *)
let hide_win (w : Window.t) =
  if Hashtbl.mem Globals.hidden w.id
  (* less: return -1? can happen if window thread take too much time
   * to respond to the Reshape command?
   *)
  then raise (Impossible "window already hidden");
  let old_layer : Display.image = w.img in
  let display = old_layer.display in
  (* this is an image! not a layer, so it will not be visible on screen *)
  let img = 
    Image.alloc display w.screenr old_layer.chans false Color.white in
  (* less: return 0 or 1 if can or can not allocate? *)
  Hashtbl.add Globals.hidden w.id w;
  let cmd = Window.Reshape img in
  Event.send w.chan_cmd cmd |> Event.sync;
  ()
(*e: function [[Wm.hide_win]] *)

(*s: function [[Wm.show_win]] *)
let show_win (w : Window.t) (desktop : Baselayer.t) =
  let old_img : Display.image = w.img in
  (* back to a layer *)
  let layer = Layer.alloc desktop old_img.r Color.white in
  Hashtbl.remove Globals.hidden w.id;
  let cmd = Window.Reshape layer in
  Event.send w.chan_cmd cmd |> Event.sync;
  ()
(*e: function [[Wm.show_win]] *)

(*****************************************************************************)
(* Helper for? *)
(*****************************************************************************)
(*s: function [[Wm.resize_win]] *)
(* less: move boolean parameter, useless opti test dx/dy below catch it *)
let resize_win (w : Window.t) (new_img : Display.image) =
  let old_img : Display.image = w.img in
  let old_r : Rectangle.t = old_img.r in
  let new_r : Rectangle.t = new_img.r in
  if Rectangle.dx old_r = Rectangle.dx new_r && 
     Rectangle.dy old_r = Rectangle.dy new_r
  then Draw.draw new_img new_r old_img None old_r.min;
  (* a layer or image, so when hiding this should make disappear the window *)
  Image.free old_img;
  (* less: screenr set in caller, but could do it here *)
  w.img <- new_img;
  (* todo: wsetname *)

  (* todo: textual window update *)
  draw_border w Window.Selected;
  incr Window.topped_counter;
  w.topped <- !Window.topped_counter;

  (* todo: w.W.resized <- true *)
  (* todo: mouse counter ++ so transmit resize event *)
  ()
(*e: function [[Wm.resize_win]] *)
(*e: Wm.ml *)
