(*s: CLI.ml *)
(* Copyright 2017-2026 Yoann Padioleau, see copyright.txt *)
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
(*s: type [[CLI.caps]] *)
(* Need:
 * - draw/mouse/keyboard because rio multiplexes access to those devices
 * - fork/exec/chdir when creating new windows which trigger new rc
 *   processes run possibly from different directories.
 * - open_in: for /dev/winname access
 * - mount/bind: for the window to mount the rio fileserver to /mnt/wsys
 *   and then bind it to /dev for virtual /dev/{cons,mouse,...}
 *)
type caps = < 
    Cap.draw; Cap.mouse; Cap.keyboard;
    Cap.fork; Cap.exec; Cap.chdir;
    Cap.open_in;
    Cap.mount; Cap.bind
  >
(*e: type [[CLI.caps]] *)

(*s: constant [[CLI.usage]] *)
let usage = 
  "usage: rio [options]"
(*e: constant [[CLI.usage]] *)

(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)
(*s: function [[CLI.thread_main]] *)
let thread_main (caps: < caps; .. >) : Exit.t =

  (* Rio, a graphical application *)
  (*s: [[CLI.thread_main()]] graphics initializations *)
  let display : Display.t = Draw.init caps "orio" in
  (* alt: simpler (but does not allow rio under rio in): 
   * let view = display.image in
   * less: let viewr save?
   *)
  let view : Display.image = Draw_rio.get_view caps display in
  let font : Font.t = Font_default.load_default_font display in

  (*s: [[CLI.thread_main()]] graphics initializations, debug *)
  if !Globals.debug_draw
  then Display.debug display;
  (*e: [[CLI.thread_main()]] graphics initializations, debug *)

  let background = Image.alloc_color display (Color.mk2 0x77 0x77 0x77) in
  Globals.red   := Image.alloc_color display (Color.mk2 0xDD 0x00 0x00);
  Globals.title_color       := Image.alloc_color display Color.greygreen;
  Globals.title_color_light := Image.alloc_color display Color.palegreygreen;

  let desktop : Baselayer.t = Baselayer.alloc view background in

  Draw.draw_color view view.r background;
    (* to test: alternative to -test that leverages work done above
    Test.test_display_default_font display view;
    Test.test_display_text display view font;
    *)

  Display.flush display;
  (*e: [[CLI.thread_main()]] graphics initializations *)

  (* Rio, a filesystem server *)
  let fs = Fileserver.init () in

  (* Rio, a concurrent application *)
  let (exit_chan: Exit.t Event.channel) = Event.new_channel () in
  (*s: [[CLI.thread_main()]] threads creation *)
  let mouse : Mouse.ctl = Mouse.init caps in
  let kbd : Keyboard.ctl = Keyboard.init caps in

  let _kbd_thread   = 
    Thread.create Thread_keyboard.thread kbd in

  let _mouse_thread = 
    Thread.create (Thread_mouse.thread caps)
       (exit_chan, 
        mouse, (display, desktop, view, font),
        fs) in

  let _fileserver_master_thread =
    Thread.create Threads_fileserver.thread fs in
  (*x: [[CLI.thread_main()]] threads creation *)
  (* To break some mutual dependencies.
   * The mouse right-click and menu will trigger the creation
   * of new windows and new window threads and call this function.
   *)
  Wm.threads_window_thread_func := Threads_window.thread;
  (*e: [[CLI.thread_main()]] threads creation *)

  (* Wait *)
  let exit_code = Event.receive exit_chan |> Event.sync in
  (* todo: kill all procs? all the winshell processes? *)
  (* todo: kill all threads? done when do exit no? *)
  exit_code
(*e: function [[CLI.thread_main]] *)
    
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: function [[CLI.main]] *)
let main (caps : < caps; Cap.stdout; Cap.stderr; ..>) (argv : string array) :
    Exit.t =
  (*s: [[CLI.main()]] locals *)
  let level = ref (Some Logs.Warning) in
  (*x: [[CLI.main()]] locals *)
  let backtrace = ref false in
  (*e: [[CLI.main()]] locals *)
  let options = [
    (*s: [[CLI.main()]] [[options]] elements *)
    "-s", Arg.Unit (fun () -> raise Todo),
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-i", Arg.String (fun _s -> raise Todo),
    " <initcmd>";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-k", Arg.String (fun _s -> raise Todo),
    " <kbdcmd>";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-font", Arg.String (fun _s -> raise Todo),
    " <fontname>";
    (*x: [[CLI.main()]] [[options]] elements *)
    (* pad: not in original *)
    "-debug_9P", Arg.Set Globals.debug_9P,
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-debug_draw", Arg.Set Globals.debug_draw,
    " ";
    (*x: [[CLI.main()]] [[options]] elements *)
    "-test", Arg.Unit (fun () -> Test.test ()),
    " ";
    (*e: [[CLI.main()]] [[options]] elements *)
  ] |> Arg.align
  in
  (* This may raise ExitCode *)
  Arg_.parse_argv caps argv options (fun _f -> Arg.usage options usage) usage;
  (*s: [[CLI.main()]] logging setup *)
  Logs_.setup !level ();
  Logs.info (fun m -> m "rio ran from %s" (Sys.getcwd()));
  (*e: [[CLI.main()]] logging setup *)
  try 
    (* the main call *)
    thread_main caps
  with exn ->
      (*s: [[CLI.main()]] handle [[exn]] *)
      if !backtrace
      then raise exn
      else 
        (match exn with
        | Failure s ->
              Logs.err (fun m -> m "%s" s);
              Exit.Code 1
        | _ -> raise exn
        )
      (*e: [[CLI.main()]] handle [[exn]] *)
(*e: function [[CLI.main]] *)
(*e: CLI.ml *)
