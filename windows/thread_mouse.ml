open Common

open Mouse
module I = Image
module W = Window

type event =
  | Mouse of Mouse.state
  (* less: Resize? other? *)

let middle_click_system m mouse =
  pr "Todo: middle click"

(* right click normally *)
let wm_menu pos button exitchan 
    mouse (display, desktop, view, font) fs =
  (* todo: set (and later restore) sweeping to true *)

  let items = [
    (* todo: the first item get selected the very first time; QEMU bug?  *)
    "New", (fun () ->
      let img_opt = Mouse_action.sweep mouse (display, desktop, view, font) in
      img_opt |> Common.if_some (fun img ->
        (* Wm.new_win img "/bin/rc" [|"rc"; "-i"|] None mouse *)
        Wm.new_win img "/tests/xxx/test_rio_graph_app1" 
          [|"/tests/xxx/test_rio_graph_app1"|] None mouse fs
      )
    );
    (* old: was Reshape but here it's really resizing *)
    "Resize", (fun () -> raise Todo);
    "Move", (fun () -> raise Todo);
    "Delete", (fun () -> 
      let wopt = Mouse_action.point_to mouse in
      wopt |> Common.if_some (fun w ->
        let cmd = W.Delete in
        Event.send w.W.chan_cmd cmd |> Event.sync;
      ));
    "Hide", (fun () -> 
      let wopt = Mouse_action.point_to mouse in
      wopt |> Common.if_some (fun w ->
        Wm.hide_win w mouse
      ));
    "Exit", (fun () ->
      Event.send exitchan 0 |> Event.sync;
    );
  ] @
  (Globals.hidden |> Hashtbl_.to_list |> List.map (fun (_wid, w) ->
    (* less: could sort *)
    w.W.label, (fun () -> 
      Wm.unhide_win w desktop mouse
    )))
  in
  (* less: adjust menu with hidden windows *)
  Menu_ui.menu items pos button
    mouse (display, desktop, view, font)



type under_mouse =
  | Nothing
  | CurrentWin of Window.t
  | OtherWin of Window.t

let thread (exitchan, 
            mouse, (display, desktop, view, font), fs) =
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
      (* todo? race on Globals.win? can change between? need store in local?*)
      let sending_to_win =
        match Globals.win () with
        | Some w ->
          (* less: logical coordinates with winput.img.r and winput.screenr *)
          let xy = m.pos in
          (* less: goto scrolling if scroll buttons *)
          let inside = Window.pt_inside_frame xy w in
          (* todo: set scrolling *)
          (* todo: set moving *)
          if false
          then raise Todo
          else
             (* less: || scrolling *)
            inside && (w.W.mouse_opened || m.buttons.left)
        | None -> false
      in
      if sending_to_win
      then begin
        Globals.win () |> Common.if_some (fun w ->
          if not (Mouse.has_click m)
          then Wm.corner_cursor_or_window_cursor w m.Mouse.pos mouse
          else Wm.window_cursor w m.Mouse.pos  mouse;
          
          (* less: send logical coordinates *)
          Event.send w.W.chan_mouse m |> Event.sync
        )
      end else begin
        let wopt = Globals.window_at_point m.pos in
        (match wopt with
        | Some w -> Wm.corner_cursor_or_window_cursor w m.Mouse.pos mouse
        | None -> Mouse.reset_cursor mouse
        );
        (* todo: if moving and buttons *)
        (* todo: set corner cursor again part2 *)

        if Mouse.has_click m
        then 
          let under_mouse =
            match wopt, Globals.win () with
            | None, _ -> Nothing
            (* less: look if w2.topped > 0? seems useless *)
            | Some w1, Some w2 when w1 == w2 -> CurrentWin w1
            | Some w, _ -> OtherWin w
          in
          (match under_mouse, m.buttons with
          (* TODO: remove, just because hard right click on QEMU *)
          | Nothing , { left = true } ->
            wm_menu m.Mouse.pos Mouse.Left exitchan 
              mouse (display, desktop, view, font) fs


          | (Nothing | CurrentWin _), { left = true } ->
            ()

          | Nothing,  { middle = true } ->
             middle_click_system m mouse
          | CurrentWin w, { middle = true } ->
            if not w.W.mouse_opened
            then middle_click_system m mouse

          | (Nothing | CurrentWin _), { right = true } ->
            wm_menu m.Mouse.pos Mouse.Right exitchan 
              mouse (display, desktop, view, font) fs

          | OtherWin w, { left = true } ->
            Wm.top_win w mouse
            (* less: should drain and wait that release up, unless winborder *)
          | OtherWin w, ({ middle = true } | { right = true}) ->
            Wm.top_win w mouse
            (* todo: should goto again, may need to send event *)
            
          | _ -> raise (Impossible "Mouse.has_click so one field is true")
          );
        (* todo: reset moving *)
        (* less: drain *)
      end
    )
  done
