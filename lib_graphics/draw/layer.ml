open Common

(* Most drawing functions takes either a layer or image as a parameter.
 * less: we could force the programmer to each time do layer.img to get
 * the image in the layer. This would be more explicit. Would
 * it prevent some genericity?
 *)
type t = Image.t

(* less: refBackup? *)
let alloc base r color =
  raise Todo

let free layer =
  raise Todo


let put_to_top layer =
  raise Todo

let put_to_bottom layer =
  raise Todo

(* todo? set_origin *)
