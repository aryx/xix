open Common

module D = Display
module I = Display

let main () =

  let display = Draw.init "Hello Draw" in

  let winname = 
    (* "window.1"  *)
    (* TODO: does not work?
    match Common.cat "/dev/winname" with
    | [x] -> x
    | xs -> failwith (spf "wrong format in /dev/winname: %s"
                        (String.concat "," xs))
    *)
    let buf = String.make 256 ' ' in
    let chan = open_in "/dev/winname" in
    let n = input chan buf 0 256 in
    if n < 256
    then String.sub buf 0 n
    else failwith "buffer too short for /dev/winname"
  in
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
