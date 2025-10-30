(* pixel coordinates *)
type t = {
  x: int;
  y: int;
}

let p x y = 
  { x = x; y = y }

let zero =
  { x = 0; y = 0 }

let binary op pt1 pt2 = 
  { x = op pt1.x pt2.x;
    y = op pt1.y pt2.y;
  }


let add pt1 pt2 =
  binary (+) pt1 pt2
let sub pt1 pt2 =
  binary (-) pt1 pt2
let mul pt1 pt2 =
  binary ( * ) pt1 pt2
let div pt1 pt2 =
  binary (/) pt1 pt2
