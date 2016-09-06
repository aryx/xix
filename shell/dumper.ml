open Common

let s_of_line x =
  let v = Meta_ast.vof_line x in
  Ocaml.string_of_v v

let s_of_cmd_sequence x =
  let v = Meta_ast.vof_cmd_sequence x in
  Ocaml.string_of_v v

let s_of_cmd x =
  let v = Meta_ast.vof_cmd x in
  Ocaml.string_of_v v
