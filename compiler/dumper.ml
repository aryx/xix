open Common

(* less: could have s_of_any_with_pos, and s_of_any_with_type *)
let s_of_any x =
  let v = Meta_ast.vof_any x in
  Ocaml.string_of_v v

