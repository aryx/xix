open Common

type wid = int


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

  (* ---------------------------------------------------------------- *)
  (* Control *)
  (* ---------------------------------------------------------------- *)

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


}

let wid_counter = 
  ref 0
let topped_counter =
  ref 0
