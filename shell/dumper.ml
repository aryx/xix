open Common

module Ocaml = OCaml

(* less: could use Any *)

let s_of_line x =
  let v = Meta_ast.vof_line x in
  Ocaml.string_of_v v

let s_of_cmd_sequence x =
  let v = Meta_ast.vof_cmd_sequence x in
  Ocaml.string_of_v v

let s_of_cmd x =
  let v = Meta_ast.vof_cmd x in
  Ocaml.string_of_v v

let s_of_value x =
  let v = Meta_ast.vof_value x in
  Ocaml.string_of_v v


let s_of_opcode x =
  let v = Meta_opcode.vof_opcode x in
  Ocaml.string_of_v v

let s_of_operation x =
  let v = Meta_opcode.vof_operation x in
  Ocaml.string_of_v v

let s_of_codevec x =
  let xs = Array.to_list x in
  let v = Ocaml.vof_list Meta_opcode.vof_opcode xs in
  Ocaml.string_of_v v
  
