(*s: Window.ml *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The data structure to store all the information about a window! *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*s: type [[Window.wid]] *)
(* window id *)
type wid = int
(*e: type [[Window.wid]] *)

(*s: type [[Window.mouse_counter]] *)
type mouse_counter = int
(*e: type [[Window.mouse_counter]] *)
(*s: type [[Window.topped_counter]] *)
type topped_counter = int
(*e: type [[Window.topped_counter]] *)

(*s: type [[Window.cmd]] *)
type cmd =
  (*s: [[Window.cmd]] cases *)
  | Delete
  (*x: [[Window.cmd]] cases *)
  (*
    | Move of Image.t * Rectangle.t
    | Refresh
    | Wakeup
    (* less: RawOff | RawOn? HoldOn | HoldOff *)
  *)
  (*x: [[Window.cmd]] cases *)
  (* for resize event but also for hide/show *)
  | Reshape of 
        Image.t (* can be Layer.t or an off-screen Image.t when hidden *)
        (*Mouse.ctl*) (* needed for window_cursor() when repaint border *)
  (*e: [[Window.cmd]] cases *)
(*e: type [[Window.cmd]] *)

(*s: type [[Window.t]] *)
(* The window type! *)
type t = {
  (* ---------------------------------------------------------------- *)
  (* ID *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] id fields *)
  (* visible in /mnt/wsys/winid (and used for /mnt/wsys/<id>/devs) *)
  id: wid;
  (* public named image, visible in /mnt/wsys/winname; change when resize *)
  mutable winname: string;
  (*x: [[Window.t]] id fields *)
  (* writable through /mnt/wsys/label *)
  mutable label: string;
  (*e: [[Window.t]] id fields *)

  (* ---------------------------------------------------------------- *)
  (* Graphics *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] graphics fields *)
  (* This is most of the time a layer, but it can also be a plain Image.t
   * when the window is hidden.
   * less: option? when delete the window structure and thread is still
   * out there because we wait for the process to terminate?
   *)
  mutable img: Image.t;
  (*x: [[Window.t]] graphics fields *)
  (* todo: for originwindow and really virtual screen? vs img.r? *)
  mutable screenr: Rectangle.t;
  (*x: [[Window.t]] graphics fields *)
  (* writable through /mnt/wsys/cursor *)
  mutable mouse_cursor: Cursor.t option;
  (*e: [[Window.t]] graphics fields *)

  (* ---------------------------------------------------------------- *)
  (* Mouse *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] mouse fields *)
  (* Note that we do not queue all mouse states; just the clicks/releases,
   * otherwise the queue would be too big when you move around the mouse.
   * less: max size = ? mutex around? recent queue.mli says not thread-safe 
   *)
  mouseclicks_queue: (Mouse.state * mouse_counter) Queue.t;
  (*x: [[Window.t]] mouse fields *)
  (* less: could have simpler mouse_new_event: bool? *)
  mutable mouse_counter: mouse_counter;
  mutable last_count_sent: mouse_counter;
  (*x: [[Window.t]] mouse fields *)
  (* we do not queue all mouse states (we queue just the clicks/releases);
   * for the rest (moving the mouse) we just keep the last state.
   *)
  mutable last_mouse: Mouse.state;
  (* ?? how differ from last_mouse.buttons? *)
  mutable last_buttons: Mouse.buttons;
  (*x: [[Window.t]] mouse fields *)
  (* Threads_window.thread <-- Thread_mouse.thread (<-- Mouse.thread) *)
  chan_mouse: Mouse.state Event.channel;
  (*x: [[Window.t]] mouse fields *)
  (* Threads_window.thread --> Thread_fileserver.dispatch(Read).
   * The channel inside the channel will be used to write a mouse state
   * to thread_fileserver.
  *)
  chan_devmouse_read: Mouse.state Event.channel Event.channel;
  (*e: [[Window.t]] mouse fields *)

  (* ---------------------------------------------------------------- *)
  (* Keyboard *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] keyboard fields *)
  (* Threads_window.thread --> Thread_fileserver.dispatch(Read).
   * The first channel will be used by thread_fileserver to indicate the
   * number of bytes the process wants to read from its /dev/cons. The second
   * channel will be used to send the bytes to thread_fileserver.
   * Note that we send bytes, even though we read keys.
  *)
  chan_devcons_read: (int Event.channel * string Event.channel) Event.channel;
  (*x: [[Window.t]] keyboard fields *)
  (* Threads_window.thread --> Thread_fileserver.dispatch(Write).
   * Note that we send full runes, not bytes.
   * The channel inside will be used to read from thread_fileserver(Write)
   * the data the process wrote to its /dev/cons.
   *)
  chan_devcons_write: (Rune.t list Event.channel) Event.channel;
  (*x: [[Window.t]] keyboard fields *)
  (* see also Window.terminal below for keys when in non-raw (buffered) mode *)
  raw_keys: Keyboard.key Queue.t;
  (*x: [[Window.t]] keyboard fields *)
  (* Threads_window.thread <-- Thread_keyboard.thread (<-- keyboard.thread) *)
  (* todo: need list of keys? [20]?not reactif enough if buffer one key only? *)
  chan_keyboard: Keyboard.key Event.channel;
  (*e: [[Window.t]] keyboard fields *)

  (* ---------------------------------------------------------------- *)
  (* Commands *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] command fields *)
  (* Threads_window.thread <-- Thread_mouse.thread? | ?? *)
  (* less: also list of cmds? [20]? *)
  chan_cmd: cmd Event.channel;
  (*e: [[Window.t]] command fields *)

  (* ---------------------------------------------------------------- *)
  (* Process *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] process fields *)
  (* not really mutable, but set after Window.alloc() *)
  mutable pid: int;
  (*x: [[Window.t]] process fields *)
  (* can be changed through /mnt/wsys/wdir *)
  mutable pwd: Fpath.t;
  (* todo? notefd *)
  (*e: [[Window.t]] process fields *)

  (* ---------------------------------------------------------------- *)
  (* Config *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] config fields *)
  mutable auto_scroll: bool;
  (*e: [[Window.t]] config fields *)

  (* ---------------------------------------------------------------- *)
  (* Wm *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] wm fields *)
  mutable topped: topped_counter;
  (*e: [[Window.t]] wm fields *)

  (* ---------------------------------------------------------------- *)
  (* Textual Window *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] textual window fields *)
  terminal: Terminal.t;
  (*e: [[Window.t]] textual window fields *)

  (* ---------------------------------------------------------------- *)
  (* Graphical Window *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] graphical window fields *)
  mutable raw_mode: bool;
  (*x: [[Window.t]] graphical window fields *)
  (* can also be used in textual windows, but more rare *)
  mutable consctl_opened: bool;
  (*x: [[Window.t]] graphical window fields *)
  mutable mouse_opened: bool;
  (*e: [[Window.t]] graphical window fields *)

  (* ---------------------------------------------------------------- *)
  (* Concurrency *)
  (* ---------------------------------------------------------------- *)
  (* less: 
   * - a Ref (Mutex.t? atomic anyway in ocaml), ref counting
   *   or simply a counter as there is no race issue for rio-ocaml.
   * - Qlock (Condition.t?), needed for?
   *)

  (* ---------------------------------------------------------------- *)
  (* Misc *)
  (* ---------------------------------------------------------------- *)
  (*s: [[Window.t]] other fields *)
  (* todo: why need this? race between delete window and other operation? *)
  mutable deleted: bool;
  (*e: [[Window.t]] other fields *)
}
(*e: type [[Window.t]] *)

(*s: global [[Window.wid_counter]] *)
let wid_counter = 
  ref 0
(*e: global [[Window.wid_counter]] *)
(*s: global [[Window.topped_counter]] *)
let topped_counter =
  ref 0
(*e: global [[Window.topped_counter]] *)

(*s: constant [[Window.window_border_size]] *)
(* important convention to follow for rio and draw to cooperate correctly *)
let window_border_size = Draw_rio.window_border_size (* 4 *)
(*e: constant [[Window.window_border_size]] *)

(*s: type [[Window.border_status]] *)
type border_status = 
  | Selected
  | Unselected
(*e: type [[Window.border_status]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Window.pt_inside_border]] *)
(* old: was not an helper in rio, but should to be consistent with winborder.
 * alt: pt_on_content (window border vs window content in Windows.nw)
 *)
let pt_inside_border pt w =
  Rectangle.pt_in_rect pt (Rectangle.insetrect window_border_size w.screenr)
(*e: function [[Window.pt_inside_border]] *)
(*s: function [[Window.pt_on_border]] *)
(* old: was called winborder in rio *)
let pt_on_border pt w =
  Rectangle.pt_in_rect pt w.screenr && not (pt_inside_border pt w)
(*e: function [[Window.pt_on_border]] *)

(*s: function [[Window.alloc]] *)
(* ... -> Thread_mouse.wm_menu -> Wm.new_win -> <> *)
let alloc (img : Display.image) (font : Font.t) : t = 
  incr wid_counter;
  incr topped_counter;

  let w = 
  { 
    id = !wid_counter;
    (* will be set in caller *)
    winname = "";
    label = "<unnamed>";

    (* set later in Wm.ml in the caller *)
    pid = -1;
    pwd = Fpath.v (Sys.getcwd ());

    img = img;
    screenr = img.r;

    topped = !topped_counter;

    mouse_opened   = false;
    consctl_opened = false;
    raw_mode       = false;

    deleted = false;

    mouse_cursor = None;

    chan_mouse    = Event.new_channel ();
    chan_keyboard = Event.new_channel ();
    chan_cmd      = Event.new_channel ();

    chan_devmouse_read = Event.new_channel ();
    mouseclicks_queue = Queue.create ();
    mouse_counter = 0;
    last_count_sent = 0;
    last_mouse = Mouse.fake_state;
    last_buttons = Mouse.nobuttons;

    chan_devcons_read  = Event.new_channel ();
    chan_devcons_write = Event.new_channel ();
    raw_keys = Queue.create ();

    terminal = Terminal.alloc img font;

    auto_scroll = false;
  }
  in
  w
(*e: function [[Window.alloc]] *)
  (* less: incref? in caller? *)
(*e: Window.ml *)
