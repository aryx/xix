open Common

module D = Display
module I = Display
module B = Baselayer

(* Most drawing functions takes either a layer or image as a parameter.
 * less: we could force the programmer to each time do layer.img to get
 * the image in the layer. This would be more explicit. Would
 * it prevent some genericity?
 *)
type t = Image.t

(* less: refBackup? _allocwindow and initial image *)
let alloc base r color =
  let display = base.B.display in
  (* less: have a display.screenimage? *)
  Image.alloc_gen display r display.D.image.I.chans false color 
    (Some base.B.id) Image.RefreshBackup


let free layer =
  Image.free layer


let put_to_top layer =
  raise Todo

let put_to_bottom layer =
  raise Todo

(* todo? set_origin *)
