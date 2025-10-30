(* Copyright 2015-2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Regexp_.Operators

module D = Display
module I = Display

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* rio and draw (Draw.getview) cooperate and must agree on a common constant *)
let window_border_size = 4

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Note that rio also relies on baselayer.ml and layer.ml, but those files
 * are not rio-specific. They can be used by regular graphical 
 * applications too (e.g., sam the editor)
 *)

(* less: refresh method parameter *)
let get_view_and_baselayer display =
  let winname = 
    (* TODO: does not work?
       match Common.cat "/dev/winname" with
    | [x] -> x
    | xs -> failwith (spf "wrong format in /dev/winname: %s"
                        (String.concat "," xs))
    *)
    let buf = Bytes.make 256 ' ' in
    let chan = open_in "/dev/winname" in
    let n = input chan buf 0 256 in
    if n < 256
    then Bytes.sub_string buf 0 n
    else failwith "buffer too short for /dev/winname"
  in
  let img = 
    try Draw_ipc.get_named_image display winname
    with Failure _ -> 
      Logs.warn (fun m -> m "failed to get named image for %s, using display.image" winname);
      display.D.image
  in
  (* todo: finalize free baselayer if pb allocate view *)
  let base = Baselayer.alloc img display.D.white in
  let r = 
    if winname =~ "noborder.*"
    then img.I.r
    else Rectangle.insetrect window_border_size img.I.r
  in
  let view = Layer.alloc base r Color.white in
  (* todo? originwindow *)

  view, base

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let get_view display =
  fst (get_view_and_baselayer display)
