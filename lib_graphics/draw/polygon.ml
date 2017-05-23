open Common
open Point
open Rectangle


(* special polygon *)
let border dst r i color sp =
  let r, sp, i =
    if i >= 0
    then r, sp, i
    else
      (* border goes outside the rectangle *)
      let r = Rectangle.insetrect i r in
      let sp = Point.add sp (Point.p i i) in
      let i = -1 in
      r, sp, i
  in

  (* horizontal bars *)
  Draw.draw dst (Rectangle.r r.min.x r.min.y      r.max.x (r.min.y + i)) 
    color None sp;
  Draw.draw dst (Rectangle.r r.min.x (r.max.y - i)  r.max.x r.max.y) 
    color None sp;
  (* vertical bars *)
  Draw.draw dst (Rectangle.r r.min.x (r.min.y + i)  (r.min.x + i) (r.max.y - i))
    color None sp;
  Draw.draw dst (Rectangle.r (r.max.x - i) (r.min.y + i) r.max.x (r.max.y - i))
    color None sp;
  ()
