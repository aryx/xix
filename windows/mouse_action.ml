open Common

module I = Display

type sweep_state = 
  (* start, no buttons *)
  | SweepInit
  (* right click *)
  | SweepRightClicked of Point.t
  (* move while holding right click *)
  | SweepMove of Point.t * Point.t * Image.t option
  (* release right click *)
  | SweepUnclicked of Image.t option

  (* error management when left or middle click or too small rectangle *)
  | SweepRescue of bool (* clicked state *) * Image.t option
  (* wait until no buttons *)
  | SweepDrain

let sweep mouse (display, desktop, _view, font) =
  (* todo: menuing? but not sweeping? *)
  Mouse.set_cursor mouse Cursors.crosscursor;

  let rec transit = function
    | SweepInit ->
      let m = Mouse.flush_and_read display mouse in
      if not (Mouse.has_click m)
      then transit SweepInit
      else 
        (* less: force exactly right click? *)
        (* todo: Mouse.Right at some point *)
        if Mouse.has_button m Mouse.Left
        then transit (SweepRightClicked m.Mouse.pos)
        else transit (SweepDrain)
    | SweepRightClicked p0 ->
      (* less: onsceen (using clipr) *)
      transit (SweepMove (p0, p0, None))
    | SweepMove (p0, p1, old_img_opt) ->
      let m = Mouse.flush_and_read display mouse in
      (match () with
      (* less: force exactly right click? *)
      (* todo: Mouse.Right at some point *)
      | _ when Mouse.has_button m Mouse.Left ->
        if m.Mouse.pos = p1
        then transit (SweepMove (p0, p1, old_img_opt))
        else
          (* less: onscreen *)
          let p1 = m.Mouse.pos in
          let r = Rectangle.canonical p0 p1 in
          if Rectangle.dx r > 5 && Rectangle.dy r > 5 then begin
            (* need that? should be transparent anyway no? *)
            let grey = Color.mk2 0xEE 0xEE 0xEE in
            (* todo? RefreshNothing? *)
            let img = Layer.alloc desktop r grey in
            old_img_opt |> Common.if_some Layer.free;
            Polygon.border img r Window.frame_border !Globals.red Point.zero;
            Display.flush display;
            transit (SweepMove (p0, p1, Some img))
          end
          else transit (SweepMove (p0, p1, old_img_opt))
      | _ when not (Mouse.has_click m) -> 
        transit (SweepUnclicked (old_img_opt))
      | _ -> 
        assert(Mouse.has_click m);
        transit (SweepRescue (true, old_img_opt))
      )
    | SweepUnclicked (old_img_opt) ->
      (match old_img_opt with
      | None -> None
      | Some old_img ->
        let r = old_img.I.r in
        if Rectangle.dx r < 100 || Rectangle.dy r < 3 * font.Font.height
        then transit (SweepRescue (false, old_img_opt))
        else begin
          (* todo? RefreshBackup? *)
          let img = Layer.alloc desktop r Color.white in
          Layer.free old_img;
          Polygon.border img r Window.frame_border !Globals.red Point.zero;
          (* todo: cornercursor? pos! *)
          (* todo? Display.flush display; *)
          (* less: moveto to force cursor update? ugly ... *)
          (* less: menuing = false *)
          (* finally!! got an image *)
          Some img
        end
      )
    | SweepRescue (clicked_state, old_img_opt) ->
      old_img_opt |> Common.if_some Layer.free;
      if clicked_state
      then transit SweepDrain
      else None
    | SweepDrain ->
      let m = Mouse.flush_and_read display mouse in
      if not (Mouse.has_click m)
      then None
      else transit SweepDrain
  in
  transit SweepInit
