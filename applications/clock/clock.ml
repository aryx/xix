open Common
module R = Rectangle

type event =
  | Mouse of Mouse.state
  | Key of Keyboard.key
  (* TODO:  Timer *)
  (* less: Resize *)

let circlept (c : Point.t) (r : int) (degress : int) : Point.t =
  let rad : float = float_of_int degress *. (Float_.pi /. 180.0) in
  Point.{
      x = int_of_float (float_of_int c.x +. cos rad *. (float_of_int r));
      y = int_of_float (float_of_int c.y -. sin rad *. (float_of_int r));
  }
  

let back = ref Image.fake_image
let hour_hand = ref Image.fake_image
let minute_hand = ref Image.fake_image
let dots = ref Image.fake_image

let tm = ref 0.0
let r : Rectangle.t ref = ref Rectangle.r_1x1

let redraw (display : Display.t) (view : Display.image) : unit =
  let ntm : float = Unix.time () in
  
  if ntm = !tm && view.r = !r
  then ()
  else begin
    let ntms : Unix.tm = Unix.localtime ntm in
    let anghr = 90 - (ntms.tm_hour * 5 + ntms.tm_min / 12) * 6 in
    let angmin = 90 - ntms.tm_min * 6 in
    tm := ntm;
    r := view.r;
    let c : Point.t = Point.div (Point.add !r.R.min !r.R.max) 2 in
    let rad = 
      if Rectangle.dx !r < Rectangle.dy !r
      then Rectangle.dx !r
      else Rectangle.dy !r
    in
    let rad = rad / 2 in
    let rad = rad - 8 in
    
    Draw.draw view view.r !back None Point.zero;
    for i = 0 to 11 do
      Ellipse.fill view (circlept c rad (i * (360 / 12)))
        2 2 !dots Point.zero;
    done;
    Line.line view c
      (circlept c ((rad * 3) / 4) angmin)
      Line.EndSquare Line.EndSquare 1 !minute_hand Point.zero;
    Line.line view c
      (circlept c (rad / 2) anghr)
      Line.EndSquare Line.EndSquare 1 !hour_hand Point.zero;
    
      
    Display.flush display
  end


(* the Keyboard.init() and Mouse.init() below create other threads *)
let thread_main (caps : < Cap.draw; Cap.open_in; Cap.keyboard; Cap.mouse; .. >) =
  let display : Display.t = Draw.init caps "clock" in
  let view : Display.image = Draw_rio.get_view caps display in

  let kbd : Keyboard.ctl = Keyboard.init caps in
  let mouse : Mouse.ctl = Mouse.init caps in

  back := Draw.alloc_mix_colors display Color.palebluegreen Color.white;
  (* TODO: was CMAP8 not rgba32 *)
  hour_hand := Image.alloc display Rectangle.r_1x1
            Channel.rgba32 true Color.darkblue;
  minute_hand := Image.alloc display Rectangle.r_1x1
            Channel.rgba32 true Color.paleblue;
  dots := Image.alloc display Rectangle.r_1x1
            Channel.rgba32 true Color.blue;
  
  (* if does not use originwindow, then need to add view.r.min *)
  let mousepos = ref (Point.add view.r.Rectangle.min (Point.p 10 10)) in

  (* TODO: timer channel and time thread *)

  redraw display view;

  while true do
    let ev : event = 
      [
        Keyboard.receive kbd |> (fun ev -> Event.wrap ev (fun x -> Key x));
        Mouse.receive mouse |> (fun ev -> Event.wrap ev (fun x -> Mouse x));
        (* TODO: timer channel *)
      ] |> Event.select
    in
    (match ev with
    | Mouse m -> 
      (* TODO: exit menu *)
      mousepos := m.pos
    | Key c ->
      if c = 'q'
      then exit 0
      else Logs.app (fun m -> m "%c" c)
    (* TODO: timer thread *)
    (* less: 
     * | Resize -> view := getwindow display
     *)
    );
    redraw display view;
  done

let _ =
  Cap.main (fun caps -> thread_main caps)
