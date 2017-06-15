open Common

module I = Display

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The data structures to store all the information about a window!
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* window id *)
type wid = int

type cmd =
  | Delete
  (* for resize event but also for hide/unhide *)
  | Reshape of 
      Image.t (* can be Layer.t or an off-screen Image.t when hidden *) *
      Mouse.ctl (* needed for window_cursor when repaint border *)
(*
  | Move of Image.t * Rectangle.t
  | Refresh
  | Wakeup
  (* less: RawOff | RawOn? HoldOn | HoldOff *)
*)

type mouse_counter = int

(* The window type! *)
type t = {
  (* ---------------------------------------------------------------- *)
  (* ID *)
  (* ---------------------------------------------------------------- *)
  (* visible in /mnt/wsys/winid (and used for /mnt/wsys/<id>/devs) *)
  id: wid;
  (* public named image, visible in /mnt/wsys/winname; change when resize *)
  mutable winname: string;

  (* writable through /mnt/wsys/label *)
  mutable label: string;

  (* ---------------------------------------------------------------- *)
  (* Graphics *)
  (* ---------------------------------------------------------------- *)
  (* This is most of the time a layer, but it also a plain Image.t when
   * the window is hidden.
   * less: option? when delete the window structure and thread is still
   * out there because we wait for the process to terminate?
   *)
  mutable img: Image.t;

  (* less: used to be equivalent to img.r *)
  mutable screenr: Rectangle.t;

  (* writable through /mnt/wsys/cursor *)
  mutable mouse_cursor: Cursor.t option;

  (* ---------------------------------------------------------------- *)
  (* Mouse *)
  (* ---------------------------------------------------------------- *)
  (* Threads_window.thread <-- Thread_mouse.thread (<-- Mouse.thread) *)
  chan_mouse: Mouse.state Event.channel;

  (* Threads_window.thread --> Thread_fileserver.dispatch(Read).
   * The channel inside the channel will be used to write a mouse state
   * to thread_fileserver.
  *)
  chan_devmouse_read: Mouse.state Event.channel Event.channel;

  (* less: max size = ? mutex around? recent queue.mli sayds not thread-safe *)
  mouseclicks_queue: (Mouse.state * mouse_counter) Queue.t;
  (* less: could have simpler mouse_new_event: bool? *)
  mutable mouse_counter: mouse_counter;
  mutable last_count_sent: mouse_counter;

  (* we do not queue all mouse states; we just queue the clicks/releases.
   * for the rest (moving the mouse) we just keep the last state.
   *)
  mutable last_mouse: Mouse.state;

  mutable last_buttons: Mouse.buttons;

  (* ---------------------------------------------------------------- *)
  (* Keyboard *)
  (* ---------------------------------------------------------------- *)
  (* Threads_window.thread <-- Thread_keyboard.thread (<-- keyboard.thread) *)
  (* todo: need list of keys? [20]?not reactif enough if buffer one key only? *)
  chan_keyboard: Keyboard.key Event.channel;

  (* Threads_window.thread --> Thread_fileserver.dispatch(Read).
   * The first channel will be used by thread_fileserver to indicate the
   * number of bytes the process wants to read from its /dev/cons. The second
   * channel will be used to send the bytes to thread_fileserver.
   * Note that we send bytes, even though we read keys.
  *)
  chan_devcons_read: (int Event.channel * bytes Event.channel) Event.channel;

  (* see also Window.text below for keys when in non-raw (buffered) mode *)
  raw_keys: Keyboard.key Queue.t;

  (* Threads_window.thread --> Thread_fileserver.dispatch(Write).
   * Note that we send full runes, not bytes.
   * The channel inside will be used to read from thread_fileserver(Write)
   * the data the process wrote to its /dev/cons.
   *)
  chan_devcons_write: (Rune.t list Event.channel) Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Command *)
  (* ---------------------------------------------------------------- *)
  (* Threads_window.thread <-- Thread_mouse.thread? | ?? *)
  (* less: also list of cmds? [20]? *)
  chan_cmd: cmd Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Resize *)
  (* ---------------------------------------------------------------- *)

  (* ---------------------------------------------------------------- *)
  (* Process *)
  (* ---------------------------------------------------------------- *)

  (* not really mutable, but set after Window.alloc *)
  mutable pid: int;
  (* can be changed through /mnt/wsys/wdir *)
  mutable pwd: Common.filename;
  (* todo? notefd *)

  (* ---------------------------------------------------------------- *)
  (* Config *)
  (* ---------------------------------------------------------------- *)
  mutable auto_scroll: bool;

  (* ---------------------------------------------------------------- *)
  (* Wm *)
  (* ---------------------------------------------------------------- *)
  mutable topped: int;

  (* ---------------------------------------------------------------- *)
  (* Graphical Window *)
  (* ---------------------------------------------------------------- *)
  mutable mouse_opened: bool;
  (* can also be used in textual windows, but more rare *)
  mutable consctl_opened: bool;
  mutable raw_mode: bool;

  (* ---------------------------------------------------------------- *)
  (* Textual Window *)
  (* ---------------------------------------------------------------- *)
  terminal: Terminal.t;

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
  mutable deleted: bool;

}

let wid_counter = 
  ref 0
let topped_counter =
  ref 0

let window_border_size = Draw_rio.window_border_size (* 4 *)

type border_status = 
  | Selected
  | Unselected

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* old: was not an helper, but should to be consistent with winborder.
 * alt: pt_on_content (window border vs window content in Windows.nw)
 *)
let pt_inside_border pt w =
  Rectangle.pt_in_rect pt (Rectangle.insetrect window_border_size w.screenr)
(* old: was called winborder *)
let pt_on_border pt w =
  Rectangle.pt_in_rect pt w.screenr && not (pt_inside_border pt w)


let alloc img font = 
  incr wid_counter;
  incr topped_counter;

  let w = 
  { 
    id = !wid_counter;
    winname = "";
    label = "<unnamed>";

    img = img;
    screenr = img.I.r;
    mouse_cursor = None;

    chan_mouse    = Event.new_channel ();
    chan_keyboard = Event.new_channel ();
    chan_cmd      = Event.new_channel ();

    chan_devmouse_read = Event.new_channel ();
    mouseclicks_queue = Queue.create ();
    last_mouse = Mouse.fake_state;
    mouse_counter = 0;
    last_count_sent = 0;
    last_buttons = Mouse.nobuttons;

    chan_devcons_read = Event.new_channel ();
    chan_devcons_write = Event.new_channel ();
    raw_keys = Queue.create ();

    terminal = Terminal.alloc img font;

    topped = !topped_counter;

    mouse_opened = false;
    consctl_opened = false;
    raw_mode = false;

    deleted = false;

    auto_scroll = false;

    pwd = Sys.getcwd ();
    pid = -1;
  }
  in
  w
  (* todo: wscrdraw? in caller? and draw(cols[BACK])? *)
  (* less: incref? in caller? *)
