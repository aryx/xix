open Common

module D = Display
module I = Display (* image type is in display.ml *)

let main (caps : < Cap.draw; .. >) =

  let display = Draw.init caps "Hello Draw" in
  let view = display.D.image in

  Display.debug display;
  Image.flush view;
  Image.flush view;
  Image.flush view;
  let font = Font_default.load_default_font display in

  let color = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgb24 true (* repl *)
      Color.magenta
  in
  let p = Point.p 10 10 in
  let r = Rectangle.r 10 10 100 100 in
  Polygon.border view r 1 color Point.zero;
  Text.string view p display.D.black Point.zero font "Hello world";
  Display.flush display;
  while true do () done


let _ =
   Cap.main (fun caps -> main caps)

