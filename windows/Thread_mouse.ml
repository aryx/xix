(* Copyright 2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type event =
  | Mouse of Mouse.state
  (* less: Resize? other? or use other thread and device? (cleaner) in which
   * no need for event type here.
   *)

type under_mouse =
  | Nothing
  | CurrentWin of Window.t
  | OtherWin   of Window.t

(*****************************************************************************)
(* Menus *)
(*****************************************************************************)

(* bind to right-click *)
let wm_menu (caps : < Cap.fork; .. >) (pos : Point.t) button (exitchan : Exit.t Event.channel) 
    (mouse : Mouse.ctl) (display, desktop, view, font) (fs : Fileserver.t) =
  (* todo: set (and later restore) sweeping to true *)

  let items = [
    (* less: the first item get selected the very first time; QEMU bug?  *)
    "New", (fun () ->
      let img_opt = Mouse_action.sweep mouse (display, desktop, font) in
      img_opt |> Option.iter (fun img ->
        (* 
           Wm.new_win img "/tests/xxx/test_rio_graph_app1" 
             [|"/tests/xxx/test_rio_graph_app1"|] None (mouse, fs, font)
           Wm.new_win img "/tests/rio/8.out" 
             [|"/tests/rio/8.out"|] None (mouse, fs, font)
           Wm.new_win img "/tests/xxx/hellorio" 
             [|"/tests/xxx/hellorio"|] None (mouse, fs, font)
           Wm.new_win img "/tests/xxx/test_rio_console_app1" 
             [|"/tests/xxx/test_rio_console_app1"|] None (mouse, fs, font)
        *)
           Wm.new_win caps img "/bin/rc" [|"rc"; "-i"|] None (mouse, fs, font)
      )
    );
    (* old: was Reshape but here it's really resizing *)
    "Resize", (fun () -> raise Todo);
    "Move", (fun () -> raise Todo);
    "Delete", (fun () -> 
      let wopt = Mouse_action.point_to mouse in
      wopt |> Option.iter (fun (w : Window.t) ->
        let cmd = Window.Delete in
        Event.send w.chan_cmd cmd |> Event.sync;
      ));
    "Hide", (fun () -> 
      let wopt = Mouse_action.point_to mouse in
      wopt |> Option.iter (fun w ->
        Wm.hide_win w
      ));
    "Exit", (fun () ->
      Event.send exitchan Exit.OK |> Event.sync;
    );
  ] @
  (Globals.hidden |> Hashtbl_.to_list |> List.map (fun (_wid, (w : Window.t)) ->
    (* less: could sort by name, or time it was put in hidden hash *)
    w.label, (fun () -> 
      Wm.show_win w desktop
    )))
  in
  Menu_ui.menu items pos button
    mouse (display, desktop, view, font)

let middle_click_system _m _mouse =
  Logs.err (fun m -> m "Todo: middle click")

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let thread (caps : < Cap.fork; .. >) (exitchan, 
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
        | Some (w : Window.t) ->
          (* less: logical coordinates with winput.img.r and winput.screenr *)
          let xy = m.pos in
          (* less: goto scrolling if scroll buttons *)
          let inside = Window.pt_inside_border xy w in
          (* todo: set scrolling *)
          (* todo: set moving *)
          (* less: || scrolling *)
          inside && (w.mouse_opened || m.buttons.left)
        | None -> false
      in
      (match sending_to_win with
      | true ->
        (* could assert that Globals.win() <> None *)
        Globals.win () |> Option.iter (fun (w : Window.t) ->
          (if not (Mouse.has_click m)
          then Wm.corner_cursor_or_window_cursor w m.pos mouse
          (* todo: why if click then not corner cursor? *)
          else Wm.window_cursor w m.pos  mouse
          );
          (* less: send logical coordinates *)
          Event.send w.chan_mouse m |> Event.sync
        )
      | false ->
        let wopt = Globals.window_at_point m.pos in
        (match wopt with
        | Some w -> Wm.corner_cursor_or_window_cursor w m.pos mouse
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
          (match under_mouse with
          (* TODO: remove; just because hard to right click on QEMU and laptop*)
          | Nothing when m.buttons.left ->
            wm_menu caps m.pos Mouse.Left exitchan 
              mouse (display, desktop, view, font) fs


          | ((*Nothing |*) CurrentWin _) when m.buttons.left ->
            ()

          | Nothing when m.buttons.middle ->
             middle_click_system m mouse
          | CurrentWin (w : Window.t) when m.buttons.middle ->
            if not w.mouse_opened
            then middle_click_system m mouse

          | (Nothing | CurrentWin _) when m.buttons.right ->
            wm_menu caps m.pos Mouse.Right exitchan 
              mouse (display, desktop, view, font) fs

          | OtherWin w when m.buttons.left ->
            Wm.top_win w
            (* less: should drain and wait that release up, unless winborder *)
          | OtherWin w when m.buttons.middle || m.buttons.right ->
            Wm.top_win w
            (* todo: should goto again, may need to send event *)
            
          | _ -> raise (Impossible "Mouse.has_click so one field is true")
          );
        (* todo: reset moving *)
        (* less: drain *)
      )
    )
  done
