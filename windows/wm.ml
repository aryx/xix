open Common

module I = Display
module W = Window

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

let set_current_and_repaint_borders w =
  (* less: if wkeyboard *)
  let old = !Globals.current in
  Globals.current := Some w;
  old |> Common.if_some (fun w2 ->
    if not (w2 == w)
    (* less: could do directly: draw_border w W.Unseleted *)
    then repaint_border w
  );
  (* less: could do directly: draw_border w W.Seleted *)
  repaint_border w;
  (* less: wsetcursor *)
  (* todo: wakeup? why? *)
  ()

let top_win w =
  raise Todo


(* less: hideit, pid, dir, scrolling *)
let new_win img cmd argv =

  (* less: cpid channel *)
  let display = img.I.display in

  (* less: scrollit *)
  let w = Window.alloc img in
  (* less: wscrdraw here? *)

  (* stricter: draw_border w Window.Selected;
   * but done already later in set_current_and_repaint_borders
   *)

  Hashtbl.add Globals.windows w.W.id w;
  let _win_thread = Thread.create Threads_window.thread w in

  (* less: if not hideit *)
  set_current_and_repaint_borders w;
  Display.flush display;

  (* todo: create a new process! *)
  (* todo: wsetname *)
  ()
