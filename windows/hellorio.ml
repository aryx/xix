open Common

module I = Image

let redraw display view loc bgcolor =
  Draw.draw view view.I.r bgcolor None Point.zero;
  (* todo: Text.string *)
  Line.line view loc (Point.add loc (Point.p 100 100))
    Line.EndSquare Line.EndSquare 2 display.I.black Point.zero;
  Display.flush display
  

let thread_main () =
  let display = Draw.init "Hello Rio" in

  let mouse = Mouse.init () in
  (*
     let kbd = 
  *)
  let bgcolor = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgba32 true Color.magenta
  in
  let mouseloc = ref (Point.p 10 10) in

  redraw display display.I.image !mouseloc bgcolor;

  while true do
    
    ()
  done

let _ =
  thread_main ()
