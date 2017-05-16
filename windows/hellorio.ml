open Common

module I = Image

type event =
  | Mouse of Mouse.t
  | Key of Keyboard.key
  (* less: Resize *)

let redraw display view loc bgcolor =
  Draw.draw view view.I.r bgcolor None Point.zero;
  (* todo: Text.string *)
  Line.line view loc (Point.add loc (Point.p 100 100))
    Line.EndSquare Line.EndSquare 2 display.I.black Point.zero;
  Display.flush display
  

let thread_main () =
  let display = Draw.init "Hello Rio" in
  (* less: getwindow? *)
  let view = display.I.image in

  let kbd = Keyboard.init () in
  let mouse = Mouse.init () in

  let bgcolor = 
    Image.alloc display (Rectangle.r 0 0 1 1) Channel.rgba32 true Color.magenta
  in
  let mouseloc = ref (Point.p 10 10) in

  redraw display view !mouseloc bgcolor;

  while true do
    let ev = 
      [
        Keyboard.receive kbd |> (fun ev -> Event.wrap ev (fun x -> Key x));
        Mouse.receive mouse |> (fun ev -> Event.wrap ev (fun x -> Mouse x));
      ] |> Event.select
    in
    (match ev with
    | Mouse m -> 
      mouseloc := m.Mouse.xy
    | Key c ->
      if c = 'q'
      then exit 0
      else pr (spf "%c" c)
    (* less: 
     * | Resize -> view := getwindow display
     *)
    );
    redraw display display.I.image !mouseloc bgcolor;
  done

let _ =
  thread_main ()
