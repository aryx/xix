open Common

let usage = 
  "usage: rio [options]"

let thread_main () =
  let display = Draw.init "Rio" in
  (* less: let view = Layer.window_init () in *)
  let view = display.Image.image in

  let kbd = Keyboard.init () in
  let mouse = Mouse.init () in

  let background = 
    Image.alloc display in
  let red = 
    Image.alloc display in
  
  let desktop = Baselayer.alloc  in
  raise Todo

let main () =
  (*
  Test.test ()
  *)
  let options = (*todo: Arg.align*) [
    "-s", Arg.Unit (fun () -> raise Todo),
    " ";
    "-font", Arg.String (fun s -> raise Todo),
    " <fontname>";
    "-i", Arg.String (fun s -> raise Todo),
    " <initcmd>";
    "-k", Arg.String (fun s -> raise Todo),
    " <kbdcmd>";
  ]
  in
  Arg.parse options (fun _ -> Arg.usage options usage) usage;
  thread_main ()
  

let _ =
  main ()
