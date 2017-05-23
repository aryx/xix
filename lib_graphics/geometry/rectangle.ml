open Point

type t = {
  min: Point.t;
  (* 'max' does not belong to the rectangle, so a rectangle from (0,0) to
   * (100,100) is actually going until (99,99). The rectangle length and
   * width are 100 though.
   *)
  max: Point.t;
}

let r x1 y1  x2 y2 =
  { min = { x = x1; y = y1; };
    max = { x = x2; y = y2; };
  }

let canonical p1 p2 =
  { min = { x = min p1.x p2.x; y = min p1.y p2.y; };
    max = { x = max p1.x p2.x; y = max p1.y p2.y; };
  }
            

let rp pt1 pt2 =
  { min = pt1; max = pt2 }

let zero =
  { min = Point.zero;
    max = Point.zero;
  }

(* width, delta x *)
let dx r = 
  r.max.x - r.min.x

(* height, delta y *)
let dy r =
  r.max.y - r.min.y

let binary_pt op r p =
  { min = { x = op r.min.x p.x; y = op r.min.y p.y; };
    max = { x = op r.max.x p.x; y = op r.max.y p.y; };
  }

(* less: add_vect? *)
let add_pt p r =
  binary_pt (+) r p

let sub_pt p r =
  binary_pt (-) r p


let pt_in_rect p r =
  p.x >= r.min.x && p.x < r.max.x &&
  p.y >= r.min.y && p.y < r.max.y

let rect_in_rect r1 r2 =
  r1.min.x >= r2.min.x && r1.max.x <= r2.max.x &&
  r1.min.y >= r2.min.y && r1.max.y <= r2.max.y

(* less: rectXrect?? *)

(* reduce if n is positive, enlarge if negative *)
let insetrect n r =
  { min = { x = r.min.x + n; y = r.min.y + n };
    max = { x = r.max.x - n; y = r.max.y - n };
  }
