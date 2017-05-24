open Common

module I = Display
module W = Window

(* less: a rio_cursor? with lastcursor opti? and force parameter? *)
let window_cursor _w mouse =
  (* TODO: use w.cursor *)
  (* less: if menuing? or use corner_cursor() so no need this global? *)
  (* less: if holding *)
  Mouse.reset_cursor mouse


let corner_cursor_or_window_cursor w pt mouse =
  if Window.pt_on_frame pt w
  then Mouse.set_cursor mouse (Cursors.which_corner_cursor w.W.screenr pt)
  else window_cursor w mouse

let corner_cursor w pt mouse =
  if Window.pt_on_frame pt w
  then Mouse.set_cursor mouse (Cursors.which_corner_cursor w.W.screenr pt)


let draw_border w status =
  let img = w.W.img in
  (* less: if holding? *)
  let color = 
    match status with
    | W.Selected   -> !Globals.title_color
    | W.Unselected -> !Globals.title_color_light
  in
  Polygon.border img img.I.r Window.frame_border color Point.zero

let repaint_border w =
  (* todo: update cols *)
  (* todo: if mouse not opened *)
  match Globals.win () with
  | Some w2 when w2 == w -> 
    draw_border w W.Selected
    (* less: wsetcursor again? *)
  | _ -> 
    draw_border w W.Unselected

let set_current_and_repaint_borders w mouse =
  (* less: if wkeyboard *)
  let old = !Globals.current in
  Globals.current := Some w;
  old |> Common.if_some (fun w2 ->
    if not (w2 == w)
    (* less: could do directly: draw_border w2 W.Unseleted *)
    (* bugfix: was doing repaint_border w, hmm *)
    then repaint_border w2
  );
  (* less: could do directly: draw_border w W.Seleted *)
  repaint_border w;
  window_cursor w mouse;
  (* todo: wakeup? why? *)
  ()

let top_win w mouse =
  if w.W.topped = !Window.topped_counter
  then ()
  else begin
    Layer.put_to_top w.W.img;
    set_current_and_repaint_borders w mouse;
    Image.flush w.W.img;

    incr Window.topped_counter;
    w.W.topped <- !Window.topped_counter;
  end



(* less: hideit, pid, dir, scrolling *)
let new_win img _cmd _argv mouse =

  (* less: cpid channel *)
  (* less: scrollit *)
  let w = Window.alloc img in
  (* less: wscrdraw here? *)

  (* simpler: draw_border w Window.Selected;
   * but done already later in set_current_and_repaint_borders
   *)

  Hashtbl.add Globals.windows w.W.id w;
  let _win_thread = Thread.create Threads_window.thread w in

  (* less: if not hideit *)
  set_current_and_repaint_borders w mouse;
  Image.flush img;

  (* todo: create a new process! *)
  (* todo: wsetname *)
  ()
