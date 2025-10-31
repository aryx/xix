(* Copyright 2017, 2025 Yoann Padioleau, see copyright.txt *)
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

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* Need: see .mli *)
type caps = < 
    Cap.draw; Cap.mouse; Cap.keyboard; 
    Cap.fork; Cap.exec; Cap.chdir;
    Cap.open_in;
    Cap.mount; Cap.bind
 >

let usage = 
  "usage: rio [options]"

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let thread_main (caps: < caps; .. >) : Exit.t =

  (* Rio, a graphical application *)

  let display : Display.t = Draw.init caps "orio" in
  (* alt: simpler (but does not allow rio under rio in): 
   * let view = display.image in
   * less: let viewr save?
   *)
  let view : Image.t = Draw_rio.get_view caps display in
  let font : Font.t = Font_default.load_default_font display in

  if !Globals.debug_draw
  then Display.debug display;

  let background = Image.alloc_color display (Color.mk2 0x77 0x77 0x77) in
  Globals.red   := Image.alloc_color display (Color.mk2 0xDD 0x00 0x00);
  Globals.title_color       := Image.alloc_color display Color.greygreen;
  Globals.title_color_light := Image.alloc_color display Color.palegreygreen;

  let desktop : Baselayer.t = Baselayer.alloc view background in

  let mouse : Mouse.ctl = Mouse.init caps in
  let kbd : Keyboard.ctl = Keyboard.init caps in

  Draw.draw_color view view.r background;
  (* to test: alternative to -test that leverages work done above
  Test.test_display_default_font display view;
  Test.test_display_text display view font;
  *)

  Display.flush display;

  (* Rio, a filesystem server *)

  let fs = Fileserver.init () in

  (* Rio, a concurrent application *)

  let _kbd_thread   = 
    Thread.create Thread_keyboard.thread kbd in

  let (exit_chan: Exit.t Event.channel) = Event.new_channel () in
  (* To break some mutual dependencies.
   * The mouse right-click and menu will trigger the creation
   * of new windows and new window threads and call this function.
   *)
  Wm.threads_window_thread_func := Threads_window.thread;
  let _mouse_thread = 
    Thread.create (Thread_mouse.thread caps) (exit_chan, 
                                       mouse, (display, desktop, view, font),
                                       fs) in

  let _fileserver_master_thread =
    Thread.create Threads_fileserver.thread fs in
  
  (* Wait *)

  let exit_code = Event.receive exit_chan |> Event.sync in
  (* todo: kill all procs? all the winshell processes? *)
  (* todo: kill all threads? done when do exit no? *)
  exit_code
    
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps; ..>) (argv : string array) : Exit.t =

  let backtrace = ref false in
  let level = ref (Some Logs.Warning) in

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
  (try
    Arg.parse_argv argv options
      (fun _f -> Arg.usage options usage) usage;
  with
  | Arg.Bad msg -> UConsole.eprint msg; raise (Exit.ExitCode 2)
  | Arg.Help msg -> UConsole.print msg; raise (Exit.ExitCode 0)
  );
  Logs_.setup !level ();
  Logs.info (fun m -> m "rio ran from %s" (Sys.getcwd()));

  try 
    (* the main call *)
    thread_main caps
  with exn ->
      if !backtrace
      then raise exn
      else 
        (match exn with
        | Failure s ->
              Logs.err (fun m -> m "%s" s);
              Exit.Code 1
        | _ -> raise exn
        )
