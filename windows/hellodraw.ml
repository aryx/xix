
let main () =
  let display = Draw.init "Hello Draw" in
(* TODO
  let color = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.RGB24 true 
      Color.magenta
  in
  let view = display.Display.image in
  Draw.draw view view.Image.r color None Point.zero;
  Draw.line view (Point.p 10 10) (Point.p 100 100)
    Line.Endsquare Line.Endsquare 10 display.Display.black Point.zero;
  Draw.string view (Point.p 200 200) display.Display.black Point.zero 
    Font.default_font "Hello Graphical World";
  Draw.flush display true;
*)
  ()


let _ =
  main ()
