(*s: tests/hellorio.ml *)
open Common

(*s: type [[Hellorio.event]] *)
type event =
  | Mouse of Mouse.state
  | Key of Keyboard.key
(*e: type [[Hellorio.event]] *)
  (* less: Resize *)

(*s: function [[Hellorio.redraw]] *)
let redraw (display : Display.t) (view : Image.t) (pos : Point.t) (bgcolor : Image.t) =
  Draw.draw view view.r bgcolor None Point.zero;
  (* todo: Text.string *)
  Line.line view pos (Point.add pos (Point.p 100 100))
    Line.EndSquare Line.EndSquare 2 display.black Point.zero;
  Display.flush display
(*e: function [[Hellorio.redraw]] *)
  

(*s: function [[Hellorio.thread_main]] *)
(* the Keyboard.init() and Mouse.init() below create other threads *)
let thread_main (caps : < Cap.draw; Cap.open_in; Cap.keyboard; Cap.mouse; .. >) =
  let display = Draw.init caps "Hello Rio" in
  let view = Draw_rio.get_view caps display in

  let kbd = Keyboard.init caps in
  let mouse = Mouse.init caps in

  let bgcolor = 
    Image.alloc display Rectangle.r_1x1 Channel.rgba32 true Color.magenta
  in
  (* if does not use originwindow, then need to add view.r.min *)
  let mousepos = ref (Point.add view.r.Rectangle.min (Point.p 10 10)) in

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
(*e: function [[Hellorio.thread_main]] *)

(*s: constant [[Hellorio._]] *)
let _ =
  Cap.main (fun caps ->
      thread_main caps
  )
(*e: constant [[Hellorio._]] *)
(*e: tests/hellorio.ml *)
