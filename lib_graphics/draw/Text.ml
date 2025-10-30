(* Copyright 2015-2017, 2025 Yoann Padioleau, see copyright.txt *)
open Common
open Point
open Rectangle

module I = Display
module M = Draw_marshal
module F = Font
module SF = Subfont
module FC = Fontchar

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: return subfontname? *)
let cache_chars font str max_n =
  (* TODO: this is a very simplified version of cache_chars that
   * assumes a specific way to load the font in Font_default.load_default_font
   *)
  assert (max_n <= String.length str);
  let sf = 
    match font.Font.subfont with
    | None -> failwith "subfont not loaded"
    | Some sf -> sf
  in
  let xs = ref [] in
  let width = ref 0 in
  for i = 0 to max_n - 1 do
    let c = str.[i] in

    (* TODO: simplified version *)
    let h = Char.code c in
    (* Common.push *)
    xs := h::!xs;
    let fc = sf.Subfont.chars.(h) in
    width := !width + fc.Fontchar.width;
  done;
  let rest_str =
    if max_n < String.length str
    then failwith (spf "cache_chars: TODO %d" max_n)
    else ""
  in
  List.rev !xs, rest_str, !width 
    

(* less: str_or_rune len bp bgp *)
let string_gen dst pt color sp font s clipr op =
  
  let s = ref s in
  let pt = ref pt in

  while String.length !s > 0 do
    (* todo: if subfontname? *)
    let xs, rest_s, width  = cache_chars font !s (min (String.length !s) 100) in
    let n = List.length xs in
    (* todo: if bg *)
    if n > 0 then begin
      (* bugfix: important to adjust pt! *)
      let adjusted_pt = {!pt with y = !pt.y + font.Font.ascent } in
      let str = "s" ^ M.bp_long dst.I.id ^ M.bp_long color.I.id ^ 
        M.bp_long font.Font.cache_img.I.id ^ 
        M.bp_point adjusted_pt ^ M.bp_rect clipr ^ M.bp_point sp ^
        M.bp_short n ^
        (xs |> List.map M.bp_short |> String.concat "")
      in
      Display.add_buf dst.I.display (Draw.adjust_str_for_op str op);

      s := rest_s;
      pt := { !pt with x = !pt.x + width };
      (* less: agefont font *)
    end
   (* less: if subfontmame *)
  done;
  !pt
    
  
(*****************************************************************************)
(* API *)
(*****************************************************************************)

let string dst pt color sp font str =
  string_gen dst pt color sp font str dst.I.clipr Draw.SoverD
(* to test ui when no font support:
  Draw.draw dst
    (Rectangle.r 
       pt.x 
       pt.y 
       (pt.x + Font.string_width font str) 
       (pt.y + font.Font.height)
    )
    src None sp
*)

(* less: str_or_rune len *)
let string_width_gen font s =

  let s = ref s in
  let total_width = ref 0 in

  while String.length !s > 0 do
    (* todo: if subfontname? *)
    let xs, rest_s, width  = cache_chars font !s (min (String.length !s) 100) in
    let n = List.length xs in
    (* todo: if cachechars failed? *)
    if n > 0 then begin
      s := rest_s;
      total_width := !total_width + width
      (* less: agefont font *)
    end
   (* less: if subfontmame *)
  done;
  !total_width

let string_width font str =
  (* to test ui when no font support:
     String.length str * 6
  *)
  string_width_gen font str

let _string_size font str =
  Point.p (string_width font str) font.Font.height
