open Common
open Point
open Rectangle

(* Graphics uses a mathematical coordinate system where the
 * origin (0, 0) is a the lower-left corner. Draw instead
 * uses a "computer" coordinate system where the origin is
 * at the upper-left corner.
 *)

let conv_point view pt = 
  pt.x, pt.y

let conv_rect view r =
  conv_point view r.min,
  conv_point view r.max
  
let conv_color color =
  raise Todo

let set_color color =
  raise Todo

let draw_rect view rect =
  let (x1,y1), (x2, y2) = conv_rect view rect in
  raise Todo
  
