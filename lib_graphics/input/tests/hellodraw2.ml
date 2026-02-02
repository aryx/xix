open Common

module I = Display

type event =
  | Mouse of Mouse.state
  | Key of Keyboard.key
  (* less: Resize *)

let redraw display view pos bgcolor =
  Draw.draw view view.I.r bgcolor None Point.zero;
  (* todo: Text.string *)
  Line.line view pos (Point.add pos (Point.p 100 100))
    Line.EndSquare Line.EndSquare 2 display.I.black Point.zero;
  Display.flush display
  

let thread_main (caps : < Cap.draw; ..>)  =
  let display = Draw.init caps "Hello Rio" in
  (* less: getwindow? *)
  let view = display.I.image in

  let kbd = Keyboard.init () in
  let mouse = Mouse.init () in

  let bgcolor = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgba32 true Color.magenta
  in
  let mousepos = ref (Point.p 10 10) in

  redraw display view !mousepos bgcolor;

  while true do
    let ev = 
      [
        Keyboard.receive kbd |> (fun ev -> Event.wrap ev (fun x -> Key x));
        Mouse.receive mouse |> (fun ev -> Event.wrap ev (fun x -> Mouse x));
      ] |> Event.select
    in
    (match ev with
    | Mouse m -> 
      mousepos := m.Mouse.pos
    | Key c ->
      if c = 'q'
      then exit 0
      else print_string (spf "%c\n" c)
    (* less: 
     * | Resize -> view := getwindow display
     *)
    );
    redraw display display.I.image !mousepos bgcolor;
  done

let _ =
  Cap.main (fun caps -> thread_main caps)
