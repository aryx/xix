open Common

let main (caps : < Cap.draw; .. >) =
  let display : Display.t = Draw.init caps "Hello Draw" in
  Display.debug display;
  let view : Display.image (* Image.t *) = display.image in
  let color : Image.t = 
    Image.alloc display Rectangle.r_1x1 Channel.rgb24 true (* repl *)
      Color.magenta
  in
  Draw.draw view view.r color None Point.zero;
  Line.line view (Point.p 10 10) (Point.p 100 100)
    Line.EndSquare Line.EndSquare 10 display.black Point.zero;
(* TODO
  Draw.string view (Point.p 200 200) display.Display.black Point.zero 
    Font.default_font "Hello Graphical World";
*)
  Display.flush display;
  Unix.sleep 3;
  ()


let _ =
  try 
    Cap.main (fun caps ->
        main caps
    )
  with Unix.Unix_error (err, s1, s2) when s2 = "xxx" ->
    failwith (spf "unix_error: %s, %s, %s" (Unix.error_message err) s1 s2)
