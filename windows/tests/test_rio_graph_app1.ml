open Common

module D = Display
module I = Display

let main () =

  let display = Draw.init "Hello Draw" in

  let winname = "window.1" in
  let view = 
    try Draw_ipc.get_named_image display winname
    with Failure _ -> 
      pr (spf "failed to get named image for %s, using display.image" winname);
      display.D.image
  in

  let color = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgb24 true (* repl *)
      Color.magenta
  in
  let r = view.I.r |> Rectangle.insetrect 20 in
  Draw.draw view r color None Point.zero;
  
  Display.flush display;
  while true do () done

let _ = 
  main ()
