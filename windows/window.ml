open Common

module I = Display

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
*)

type t = {
  (* ---------------------------------------------------------------- *)
  (* ID *)
  (* ---------------------------------------------------------------- *)
  (* visible in /mnt/wsys/winid (and used for /mnt/wsys/<id>/devs) *)
  id: wid;
  (* public named image, visible in /mnt/wsys/winname, change when resize *)
  mutable winname: string;

  (* writable through /mnt/wsys/label *)
  mutable label: string;

  (* ---------------------------------------------------------------- *)
  (* Graphics *)
  (* ---------------------------------------------------------------- *)
  (* todo: option? when delete the window structure and thread is still
   * out there because we wait for the process to terminate?
   * This is most of the time a layer, but it also a plain Image.t when
   * the window is hidden.
   *)
  mutable img: Image.t;

  (* less: used to be equivalent to img.r *)
  mutable screenr: Rectangle.t;

  (* writable through /mnt/wsys/cursor *)
  mutable mouse_cursor: Cursor.t option;

  (* ---------------------------------------------------------------- *)
  (* Mouse *)
  (* ---------------------------------------------------------------- *)
  chan_mouse: Mouse.state Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Keyboard *)
  (* ---------------------------------------------------------------- *)
  (* todo: need list of keys? [20]?not reactif enough if buffer one key only? *)
  chan_keyboard: Keyboard.key Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Command *)
  (* ---------------------------------------------------------------- *)
  (* less: also list of cmds? [20]? *)
  chan_cmd: cmd Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Process *)
  (* ---------------------------------------------------------------- *)

  (* can be changed? through /mnt/wsys/wdir *)
  mutable pwd: Common.filename;
  mutable pid: int;

  (* ---------------------------------------------------------------- *)
  (* Config *)
  (* ---------------------------------------------------------------- *)
  mutable auto_scroll: bool;

  (* ---------------------------------------------------------------- *)
  (* Wm *)
  (* ---------------------------------------------------------------- *)
  mutable topped: int;

  (* ---------------------------------------------------------------- *)
  (* Textual Window *)
  (* ---------------------------------------------------------------- *)
  (* growing array *)
  mutable text: Rune.t array;
  (* number of runes in window *)
  mutable nrunes: int;

  (* where entered text go (and selection start) (q0 in rio-C) *)
  mutable text_cursor: Editor.cursor;
  mutable end_selection: Editor.cursor option; (* q1 in rio-C) *)

  (* Division between characters the host has seen and characters not 
   * yet transmitted. The position in the text that separates 
   * output from input.
   *)
  mutable output_point: Editor.cursor;

  mutable frame: Frame_ui.t;
  mutable scrollr: Rectangle.t;
  
  (* ---------------------------------------------------------------- *)
  (* Graphical Window *)
  (* ---------------------------------------------------------------- *)
  mutable mouse_opened: bool;

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

let frame_border = 4

type border_status = 
  | Selected
  | Unselected

(* old: was not an helper, but should to be consistent with winborder *)
let pt_inside_frame pt w =
  Rectangle.pt_in_rect pt (Rectangle.insetrect frame_border w.screenr)
(* old: was called winborder *)
let pt_on_frame pt w =
  Rectangle.pt_in_rect pt w.screenr && not (pt_inside_frame pt w)


let alloc img = 
  incr wid_counter;
  incr topped_counter;

  let w = 
  { 
    id = !wid_counter;
    winname = "TODO";
    label = "<unnamed>";

    img = img;
    screenr = img.I.r;
    mouse_cursor = None;

    chan_mouse    = Event.new_channel ();
    chan_keyboard = Event.new_channel ();
    chan_cmd      = Event.new_channel ();

    topped = !topped_counter;

    text = [||];
    nrunes = 0;

    text_cursor = 0;
    end_selection = None;
    output_point = 0;

    frame = ();
    scrollr = Rectangle.r_empty;

    mouse_opened = false;
    deleted = false;

    auto_scroll = false;

    pwd = Sys.getcwd ();
    pid = -1;
  }
  in
  w
  (* todo: wscrdraw *)
  (* less: incref? *)


