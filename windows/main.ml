open Common

module I = Image

let usage = 
  "usage: rio [options]"

let thread_main () =

  (* Rio, a graphical application *)

  let display = Draw.init "Rio" in
  (* less: let view = Layer.window_init () in *)
  let view = display.Image.image in
  (* less: let viewr save? *)

  let background = 
    Image.alloc_color display (Color.mk2 0x77 0x77 0x77) in
  let red = 
    Image.alloc_color display (Color.mk2 0xDD 0x00 0x00) in

  let mouse = Mouse.init () in
  let kbd = Keyboard.init () in

  (*let desktop = Baselayer.alloc view background in*)
  Draw.draw view view.I.r background None Point.zero;
  Display.flush display;

  (* Rio, a concurrent application *)

  let (exit_chan: int (* exit code *) Event.channel) = Event.new_channel () in

  let _kbd_thread   = 
    Thread.create Thread_keyboard.thread kbd in
  let _mouse_thread = 
    Thread.create Thread_mouse.thread mouse in

  (* Rio, a filesystem server *)
  let fs = Fs.init () in

  (* Wait *)

  let exit_code = Event.receive exit_chan |> Event.sync in
  (* todo: kill all procs? all the winshell processes? *)
  (* todo: kill all threads? done when do exit no? *)
  exit exit_code
    

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

    (* pad: not in original *)
    "-debug", Arg.Set Globals.debug,
    " ";
  ]
  in
  Arg.parse options (fun _ -> Arg.usage options usage) usage;
  thread_main ()
  

let _ =
  main ()
