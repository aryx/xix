module I = Display

let test_print () =
  let x = 1+1 in
  let s = Printf.sprintf "hello world %d\n" x in
  print_string s

let test_threads_cooperatively () =
  let xt1 = ref None in
  let xt2 = ref None in

  let t1 = Thread.create (fun () ->
    print_string "thread 1\n";
    Thread.sleep ();
    print_string "thread 1 bis\n";
    !xt2 |> Common.if_some (fun t2 -> Thread.wakeup t2);
  ) () in
  xt1 := Some t1;
  let t2 = Thread.create (fun () ->
    print_string "thread 2\n";
    !xt1 |> Common.if_some (fun t1 -> Thread.wakeup t1);
    Thread.sleep ();
    print_string "thread 2 bis\n";
  ) () in
  xt2 := Some t2;
  Thread.sleep ()


let test_threads_preemptively () =

  let loop () = 
    while true do 
      for i = 0 to 1000 do
        ()
      done
    done
  in

  let t1 = Thread.create (fun () ->
    print_string "thread 1\n";
    loop ()
  ) () in
  let t2 = Thread.create (fun () ->
    print_string "thread 2\n";
    loop ()
  ) () in

  print_string "main thread\n";
  flush stdout;
  loop ()

let test_display_default_font display view =
  let subfont = Font_default.load_default_subfont display in
  let img = subfont.Subfont.bits in
  Draw.draw view img.I.r img None Point.zero;
  Image.flush view;
  ()

let test_display_text display view font =
  Display.debug display;
  Image.flush view;
  Image.flush view;
  let p = Point.p 10 10 in
  let r = Rectangle.r 10 10 100 100 in
  Polygon.border view r 1 !Globals.title_color Point.zero;
  Text.string view p !Globals.red Point.zero font "Hello world";
  Image.flush view;
  while true do () done
  

let test () =
(*
  test_print ();
  test_threads_cooperatively ();
*)
  test_threads_preemptively ();
  ()

