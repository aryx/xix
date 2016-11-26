open Common

(* less: could have s_of_any_with_pos *)
let s_of_any x =
  let v = Meta_ast.vof_any x in
  Ocaml.string_of_v v

