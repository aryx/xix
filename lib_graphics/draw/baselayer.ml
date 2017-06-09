open Common

module I = Display
module M = Draw_marshal

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Base layer on top of which other layers (see layer.ml) can be drawn.
 *
 * We could rename this file desktop.ml, but the code below provides 
 * a general mechanism that can be used not only for windowing system 
 * (e.g., in GIMP for layers)
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type t = {
  id: int;
  (* those images are stored server-side and never used client side 
   * so we don't need to keep a reference on them here
   * base: Image.t;
   * fill: Image.t;
   *)
  display: Display.t;
}

let fake_baselayer = { id = -1; display = Display.fake_display }

let counter_id = 
  ref 0

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let alloc img background =
  (* less: public bool parameter? *)
  let public = true in

  let display = img.I.display in
  if not (background.I.display == display)
  then failwith "Baselayer.alloc: image and background on different displays";

  Display.flush_buffer display;

  (* baselayer can be public and shared among multiple processes
   * so we may need multiple tries to find a free id
   *)
  let ok = ref false in
  let try_ = ref 0 in
  incr counter_id;
  while not !ok && !try_ < 25 do
    let str = "A" ^ M.bp_long !counter_id ^ 
      M.bp_long img.I.id ^ M.bp_long background.I.id ^
      M.bp_bool public 
    in
    Display.add_buf display str;
    try 
      Display.flush_buffer display;
      ok := true
    with Failure _ -> 
      incr try_;
      incr counter_id;
  done;
  { id = !counter_id;
    display = display;
  }



let free baselayer =
  failwith "Baselayer.free: TODO"

(* less: let get_public  *)
