open Common

type t = {
  (* the "screen" (or "view" when run inside a window) *)
  image: Image.t;
}

let init () =
  let _ = Graphics.open_graph " 1000x1000" in
  let r = Rectangle.r 0 0 (Graphics.size_x()) (Graphics.size_y()) in
  { image = 
      { Image.r = r;
        kind = Image.Screen;
      }
  }
