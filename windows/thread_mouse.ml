open Common
open Mouse

module I = Image
module W = Window

type event =
  | Mouse of Mouse.t
  (* less: Resize? other? *)

let middle_click_system m mouse =
  failwith "Todo: middle click"

let right_click_system m mouse =
  failwith "Todo: right click"


type under_mouse =
  | Nothing
  | CurrentWin of Window.t
  | OtherWin of Window.t

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
      (* todo? race on Globals.win? can change between? need store in local?*)
      let sending_to_win =
        match Globals.win () with
        | Some w ->
          (* less: logical coordinates with winput.img.r and winput.screenr *)
          let xy = m.xy in
          (* less: goto scrolling if scroll buttons *)
          let inside = Window.pt_inside_frame xy w in
          (* todo: set scrolling *)
          (* todo: set moving *)
          if false
          then raise Todo
          else
             (* less: || scrolling *)
            inside && (w.W.mouseopen || m.buttons.left)
        | None -> false
      in
      if sending_to_win
      then begin
        (* todo: set cursor *)
        Globals.win () |> Common.if_some (fun w ->
          (* less: send logical coordinates *)
          Event.send w.W.chan_mouse m |> Event.sync
        )
      end else begin
        let wopt = Windows.window_at_point m.xy in
        (* todo: set corner cursor if on corner part1 else riosetcursor *)
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
          | (Nothing | CurrentWin _), { left = true } -> 
            ()
          | Nothing,  { middle = true } ->
             middle_click_system m mouse
          | CurrentWin w, { middle = true } ->
            if not w.W.mouseopen
            then middle_click_system m mouse
          | (Nothing | CurrentWin _), { right = true } ->
            right_click_system m mouse

          | OtherWin w, { left = true } ->
            Wm.top_win w
            (* less: should drain and wait that release up, unless winborder *)
          | OtherWin w, ({ middle = true } | { right = true}) ->
            Wm.top_win w
            (* todo: should goto again *)
            
          | _ -> raise (Impossible "Mouse.has_click so one field is true")
          );
        (* todo: reset moving *)
        (* less: drain *)
      end
    )
  done

