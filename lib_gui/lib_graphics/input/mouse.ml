open Common
open Point

(*****************************************************************************)
(* Mouse state *)
(*****************************************************************************)

type state = {
  pos: Point.t;
  buttons: buttons;
  (* ?? *)
  msec: int;
}
  (* rio does not use the possibility to click on multiple buttons at the same
   *  time, but some applications might, so we need to provide the whole state.
   * less: opti: bitset
   *)
  and buttons = { left: bool; middle: bool; right: bool; }

let nobuttons = 
  { left = false; middle = false; right = false }

(* this type is sometimes more convenient to use *)
type button = Left | Middle | Right

let has_click m = 
  let buttons = m.buttons in
  buttons.left || buttons.middle || buttons.right

let has_button m button =
  let buttons = m.buttons in
  match button with
  | Left -> buttons.left
  | Middle -> buttons.middle
  | Right -> buttons.right

let mk pos button = 
  { pos = pos;
    buttons = 
      (match button with
      | Left -> { nobuttons with left = true }
      | Middle -> { nobuttons with middle = true }
      | Right -> { nobuttons with right = true }
      );
    msec = -1;
  }

(*****************************************************************************)
(* Parsers *)
(*****************************************************************************)
       
let buttons_of_int i =
  (* less: sanity check between 0 and 7 *)
  { left   = (i land 1) <> 0; 
    middle = (i land 2) <> 0; 
    right  = (i land 4) <> 0;
  }

let int_of_buttons buttons =
  (if buttons.left then 1 else 0) lor
  (if buttons.middle then 2 else 0) lor
  (if buttons.right then 4 else 0)
