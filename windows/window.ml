open Common

module I = Display

type wid = int

type cmd =
  | Resize of unit
  | Move of unit
  | Refresh
  | Wakeup


type t = {
  (* ---------------------------------------------------------------- *)
  (* ID *)
  (* ---------------------------------------------------------------- *)
  (* visible in /mnt/wsys/winid *)
  id: wid;
  (* visible in /mnt/wsys/winname *)
  name: string;

  (* writable through /mnt/wsys/label *)
  mutable label: string;

  (* ---------------------------------------------------------------- *)
  (* Graphics *)
  (* ---------------------------------------------------------------- *)
  mutable img: Layer.t;

  (* less: used to be equivalent to img.r *)
  screenr: Rectangle.t;

  (* ---------------------------------------------------------------- *)
  (* Mouse *)
  (* ---------------------------------------------------------------- *)
  chan_mouse: Mouse.state Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Keyboard *)
  (* ---------------------------------------------------------------- *)
  (* todo: need list of keys? not reactif enough if buffer one key only? *)
  chan_keyboard: Keyboard.key Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Command *)
  (* ---------------------------------------------------------------- *)
  chan_cmd: cmd Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Process *)
  (* ---------------------------------------------------------------- *)

  (* dir: Common.filename; *)

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
  mutable text: Editor.rune array;
  (* number of runes in window *)
  mutable nrunes: int;

  (* where entered text go (and selection start) (q0 in rio-C) *)
  mutable cursor: Editor.cursor;
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
  (* less: a Ref (Mutex.t? atomic anyway in ocaml), a Qlock (Condition.t?) *)

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

let pt_inside_frame pt w =
  Rectangle.pt_in_rect pt (Rectangle.insetrect frame_border w.screenr)
let pt_on_frame pt w =
  Rectangle.pt_in_rect pt w.screenr && not (pt_inside_frame pt w)


let alloc img = 
  incr wid_counter;
  incr topped_counter;

  let w = 
  { 
    id = !wid_counter;
    name = "TODO";
    label = "<unnamed>";

    img = img;
    screenr = img.I.r;

    chan_mouse    = Event.new_channel ();
    chan_keyboard = Event.new_channel ();
    chan_cmd      = Event.new_channel ();

    topped = !topped_counter;

    text = [||];
    nrunes = 0;

    cursor = 0;
    end_selection = None;
    output_point = 0;

    frame = ();
    scrollr = Rectangle.zero;

    mouse_opened = false;
    deleted = false;

    auto_scroll = false;
  }
  in
  w
  (* todo: wscrdraw *)
  (* less: incref? *)


