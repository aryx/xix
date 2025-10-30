(* Copyright 2015-2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common

module D = Display
module I = Display
module B = Baselayer
module M = Draw_marshal

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* Most drawing functions takes either a layer or image as a parameter.
 * less: we could force the programmer to each time do layer.img to get
 * the image in the layer. This would be more explicit. Would
 * it prevent some genericity?
 *)
type t = Image.t

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* less: refBackup? _allocwindow and initial image *)
let alloc base r color =
  let display = base.B.display in
  (* less: have a display.screenimage? *)
  Image.alloc_gen display r display.D.image.I.chans false color 
    (Some base.B.id) Image.RefreshBackup


let free layer =
  Image.free layer

type direction = Up | Down

(* less: more general and have a list? *)
let stack_op_gen layer dir =
  if layer.I.baseid = None
  then failwith "the image is not a layer";

  let display = layer.I.display in
  (* less: not sure need more general *)
  let n = 1 in
  let str = "t" ^ M.bp_bool (match dir with Up -> true | Down -> false) ^
    M.bp_short n ^ M.bp_long layer.I.id
  in
  Display.add_buf display str;
  ()


  

let put_to_top layer =
  stack_op_gen layer Up

let put_to_bottom layer =
  stack_op_gen layer Down

(* todo? set_origin *)
