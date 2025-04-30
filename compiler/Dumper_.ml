
(* less: could have s_of_any_with_pos *)
let s_of_any x =
  Meta_ast.show_types := false;
  Meta_ast.show_all_pos := false;
  let v = Meta_ast.vof_any x in
  OCaml.string_of_v v

let s_of_any_with_types x =
  Meta_ast.show_types := true;
  Meta_ast.show_all_pos := false;
  let v = Meta_ast.vof_any x in
  OCaml.string_of_v v
