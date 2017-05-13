open Common

let main () =
  pr "Let's go";
  let display = Draw.init "Hello Draw" in
  let view = display.Image.image in
  let color = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgb24 true (* repl *)
      Color.magenta
  in
(* TODO
  Draw.draw view view.Image.r color None Point.zero;
  Draw.line view (Point.p 10 10) (Point.p 100 100)
    Line.Endsquare Line.Endsquare 10 display.Display.black Point.zero;
  Draw.string view (Point.p 200 200) display.Display.black Point.zero 
    Font.default_font "Hello Graphical World";
*)
  Display.flush display;
  Unix.sleep 5;
  ()


let _ =
  main ()
