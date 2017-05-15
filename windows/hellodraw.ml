open Common

let main () =
  let display = Draw.init "Hello Draw" in
  Display.debug display;
  let view = display.Image.image in
  let color = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgb24 true (* repl *)
      Color.magenta
  in
  Draw.draw view view.Image.r color None Point.zero;
  Line.line view (Point.p 10 10) (Point.p 100 100)
    Line.EndSquare Line.EndSquare 10 display.Image.black Point.zero;
(* TODO
  Draw.string view (Point.p 200 200) display.Display.black Point.zero 
    Font.default_font "Hello Graphical World";
*)
  Display.flush display;
  Unix.sleep 3;
  ()


let _ =
  try 
    main ()
  with Unix.Unix_error (err, s1, s2) when s2 = "xxx" ->
    failwith (spf "unix_error: %s, %s, %s" (Unix.error_message err) s1 s2)

