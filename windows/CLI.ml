(* Copyright 2017 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An OCaml port of rio, the Plan 9 windowing system.
 *
 * Main limitations compared to rio:
 *  - no unicode
 *  - just a basic ASCII font for now
 *  - the terminal does not have many features
 * 
 * todo:
 *  - more fonts
 *  - unicode
 *  - need to disable preempt?
 *)

let usage = 
  "usage: rio [options]"

let thread_main () =

  (* Rio, a graphical application *)

  let display = Draw.init "Rio" in
  let view = Draw_rio.get_view display in
  (* simpler (but does not allow rio under rio in): 
   *  let view = display.I.image 
   *)
  (* less: let viewr save? *)
  let font = Font_default.load_default_font display in

  if !Globals.debug_draw
  then Display.debug display;

  let background = Image.alloc_color display (Color.mk2 0x77 0x77 0x77) in
  Globals.red   := Image.alloc_color display (Color.mk2 0xDD 0x00 0x00);
  Globals.title_color       := Image.alloc_color display Color.greygreen;
  Globals.title_color_light := Image.alloc_color display Color.palegreygreen;

  let desktop = Baselayer.alloc view background in

  let mouse = Mouse.init () in
  let kbd = Keyboard.init () in

  Draw.draw_color view view.Display.r background;
  (* to test: alternative to -test that leverages work done above
  Test.test_display_default_font display view;
  Test.test_display_text display view font;
  *)

  Display.flush display;

  (* Rio, a filesystem server *)

  let fs = Fileserver.init () in

  (* Rio, a concurrent application *)

  (* to break some mutual dependencies *)
  Wm.threads_window_thread_func := Threads_window.thread;

  let (exit_chan: int (* exit code *) Event.channel) = Event.new_channel () in

  let _kbd_thread   = 
    Thread.create Thread_keyboard.thread kbd in
  let _mouse_thread = 
    Thread.create Thread_mouse.thread (exit_chan, 
                                       mouse, (display, desktop, view, font),
                                       fs) in

  let _fileserver_master_thread =
    Thread.create Threads_fileserver.thread fs in
  
  (* Wait *)

  let exit_code = Event.receive exit_chan |> Event.sync in
  (* todo: kill all procs? all the winshell processes? *)
  (* todo: kill all threads? done when do exit no? *)
  exit exit_code
    

let main () =
  let options = (*todo: Arg.align*) [
    "-s", Arg.Unit (fun () -> raise Todo),
    " ";
    "-font", Arg.String (fun _s -> raise Todo),
    " <fontname>";

    "-i", Arg.String (fun _s -> raise Todo),
    " <initcmd>";
    "-k", Arg.String (fun _s -> raise Todo),
    " <kbdcmd>";

    (* pad: not in original *)
    "-debug_9P", Arg.Set Globals.debug_9P,
    " ";
    "-debug_draw", Arg.Set Globals.debug_draw,
    " ";
    "-test", Arg.Unit (fun () -> Test.test ()),
    " ";
  ] |> Arg.align
  in
  Arg.parse options (fun _ -> Arg.usage options usage) usage;
  thread_main ()
