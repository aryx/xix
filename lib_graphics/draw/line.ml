open Common

module I = Display (* image type is in display.ml *)
module M = Draw_marshal

type end_line =
  | EndSquare
  | EndDisc
  | EndArrow

let bp_end_line = function
  | EndSquare -> M.bp_long 0
  | EndDisc   -> M.bp_long 1
  | EndArrow  -> M.bp_long 2


let line_gen dst p0 p1 end0 end1 radius src sp op =
  let str = "L" ^ M.bp_long dst.I.id ^ M.bp_point p0 ^ M.bp_point p1 ^
    bp_end_line end0 ^ bp_end_line end1 ^ 
    M.bp_long radius ^
    M.bp_long src.I.id ^ M.bp_point sp
  in
  Display.add_buf dst.I.display (Draw.adjust_str_for_op str op)


let line dst p0 p1 end0 end1 radius src sp =
  line_gen dst p0 p1 end0 end1 radius src sp Draw.SoverD
