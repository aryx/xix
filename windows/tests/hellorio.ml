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
  

(* the Keyboard.init() and Mouse.init() below create other threads *)
let thread_main caps =
  let display = Draw.init caps "Hello Rio" in
  let view = Draw_rio.get_view caps display in

  let kbd = Keyboard.init caps in
  let mouse = Mouse.init caps in

  let bgcolor = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgba32 true Color.magenta
  in
  (* if does not use originwindow, then need to add view.r.min *)
  let mousepos = ref (Point.add view.I.r.Rectangle.min (Point.p 10 10)) in

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
      else Logs.app (fun m -> m "%c" c)
    (* less: 
     * | Resize -> view := getwindow display
     *)
    );
    redraw display view !mousepos bgcolor;
  done

let _ =
  Cap.main (fun caps ->
      thread_main caps
  )
