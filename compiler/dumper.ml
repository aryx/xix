open Common

let s_of_program x =
  let v = Meta_ast.vof_program x in
  Ocaml.string_of_v v

