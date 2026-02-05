(*s: Globals.ml *)
(* Copyright 2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common

module W = Window

(*s: global [[Globals.windows]] *)
let windows: (Window.wid, Window.t) Hashtbl.t = Hashtbl_.create ()
(*e: global [[Globals.windows]] *)
(*s: global [[Globals.hidden]] *)
(* a subset of 'windows' *)
let hidden: (Window.wid, Window.t) Hashtbl.t = Hashtbl_.create ()
(*e: global [[Globals.hidden]] *)

(*s: global [[Globals.current]] *)
(* the man page of rio (rio(1)) uses the term 'current' 
 * old: was called 'input' in rio
 *)
let current: Window.t option ref  = ref None
(*e: global [[Globals.current]] *)

(*s: function [[Globals.win]] *)
(* a bit like cpu(), up() in the kernel, a convenient global *)
let win () =
  !current
(*e: function [[Globals.win]] *)

(*s: function [[Globals.window_at_point]] *)
(* old: was called wpointto in rio *)
let window_at_point pt =
  let res = ref None in
  windows |> Hashtbl.iter (fun _k w ->
    if Rectangle.pt_in_rect pt w.W.screenr && not w.W.deleted
    then
      match !res with
      | None -> res := Some w
      | Some w2 when w.W.topped > w2.W.topped -> res := Some w
      | _ -> ()
  );
  !res
(*e: function [[Globals.window_at_point]] *)

(*s: constant [[Globals.debug_9P]] *)
let debug_9P = ref false
(*e: constant [[Globals.debug_9P]] *)
(*s: constant [[Globals.debug_draw]] *)
let debug_draw = ref false
(*e: constant [[Globals.debug_draw]] *)

(* less: could be in global, or could pass it around so more functional
 * mousectl
 * kbdctl
 * fs
 * 
 * less: could be in global, or could pass it explicitely so more functional
 * let display = ref Display.fake_display
 * let view = ref Display.fake_image
 * let font = ref Font.fake_font
 * let desktop = ref Baselayer.fake_baselayer
 *)

(* this is just too annoying to pass around *)
(*s: constant [[Globals.red]] *)
let red               = ref Display.fake_image
(*e: constant [[Globals.red]] *)
(*s: constant [[Globals.title_color]] *)
let title_color       = ref Display.fake_image
(*e: constant [[Globals.title_color]] *)
(*s: constant [[Globals.title_color_light]] *)
let title_color_light = ref Display.fake_image
(*e: constant [[Globals.title_color_light]] *)
(*e: Globals.ml *)
