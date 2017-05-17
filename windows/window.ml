open Common

type wid = int

type cmd = unit

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
  (* a layer? *)
  mutable img: Image.t;

  (* less: used to be equivalent to img.r *)
  screenr: Rectangle.t;

  (* ---------------------------------------------------------------- *)
  (* Mouse *)
  (* ---------------------------------------------------------------- *)

  (* ---------------------------------------------------------------- *)
  (* Keyboard *)
  (* ---------------------------------------------------------------- *)
  (* todo: need list of keys? not reactif enough if buffer one key only? *)
  chan_keyboard: Keyboard.key Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Control *)
  (* ---------------------------------------------------------------- *)
  chan_mouse: Mouse.t Event.channel;

  (* ---------------------------------------------------------------- *)
  (* Process *)
  (* ---------------------------------------------------------------- *)

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

  mutable frame: Frame.t;
  mutable scrollr: Rectangle.t;
  
  (* ---------------------------------------------------------------- *)
  (* Graphical Window *)
  (* ---------------------------------------------------------------- *)
  mutable mouseopen: bool;

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

let pt_inside_frame pt w =
  Rectangle.pt_in_rect pt (Rectangle.insetrect w.screenr frame_border)
let pt_on_frame pt w =
  Rectangle.pt_in_rect pt w.screenr && not (pt_inside_frame pt w)
