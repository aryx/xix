open Common

module I = Image
module W = Window

type event =
  | Mouse of Mouse.t
  (* less: Resize? other? *)


let thread mouse =
  (* less: threadsetname *)

  while true do
    let ev = 
      [
        Mouse.receive mouse |> (fun ev -> Event.wrap ev (fun x -> Mouse x));
      ] |> Event.select
    in
    (match ev with
    | Mouse m ->
      (* less: wkeyboard and button 6 *)
      (* less: loop again *)
      (* less: wkeyboard and ptinrect *)
      let sending =
        match Globals.win () with
        | Some w ->
        (* todo: logical coordinates with winput.img.r and winput.screenr *)
          let xy = m.Mouse.xy in
          (* less: goto scrolling if scroll buttons *)
          let inside = 
            Rectangle.pt_in_rect xy
              (Rectangle.insetrect w.W.screenr Window.frame_border)
          in
          (* todo: set scrolling *)
          (* todo: set moving *)
          if false
          then raise Todo
          else
             (* less: || scrolling *)
            inside && (m.Mouse.buttons.Mouse.left || w.W.mouseopen)
        | None -> false
      in
      if sending
      then begin
        (* todo: set cursor *)
        Globals.win () |> Common.if_some (fun win ->
          Event.send win.W.chan_mouse m |> Event.sync
        )
      end else begin
        let wopt = Windows.window_at_point m.Mouse.xy in
        (* todo: set corner cursor if on corner part1 else riosetcursor *)
        (* todo: if moving and buttons *)
        (* todo: set corner cursor again part2 *)

        (* todo: if buttons *)
        if Mouse.has_click m
        then begin
          raise Todo
        end
        (* todo: reset moving *)
        (* less: drain *)
      end
    )
  done

