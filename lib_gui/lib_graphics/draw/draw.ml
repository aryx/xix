open Common

module I = Image

let init label =
  let display = Display.init () in
  Graphics.set_window_title label;
  display


let draw dst r src mask pt =
  if mask <> None ||
     pt <> Point.zero
  then failwith "Draw.draw: operation not permitted yet";
  match dst.I.kind, src.I.kind with
  | I.Screen, I.Color color ->
    Draw_graphics.set_color color;
    Draw_graphics.draw_rect dst dst.I.r;
  | _ -> 
    failwith "Draw.draw: operation not permitted yet";
