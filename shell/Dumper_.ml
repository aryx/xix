(*s: shell/Dumper_.ml *)
module Ocaml = OCaml
(* less: could use Any *)

(*s: function [[Dumper_.s_of_line]] *)
let s_of_line x =
  let v = Meta_ast.vof_line x in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_line]] *)
(*s: function [[Dumper_.s_of_cmd_sequence]] *)
let s_of_cmd_sequence x =
  let v = Meta_ast.vof_cmd_sequence x in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_cmd_sequence]] *)
(*s: function [[Dumper_.s_of_cmd]] *)
let s_of_cmd x =
  let v = Meta_ast.vof_cmd x in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_cmd]] *)
(*s: function [[Dumper_.s_of_value]] *)
let s_of_value x =
  let v = Meta_ast.vof_value x in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_value]] *)

(*s: function [[Dumper_.s_of_opcode]] *)
let s_of_opcode x =
  let v = Meta_opcode.vof_opcode x in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_opcode]] *)
(*s: function [[Dumper_.s_of_operation]] *)
let s_of_operation x =
  let v = Meta_opcode.vof_operation x in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_operation]] *)
(*s: function [[Dumper_.s_of_codevec]] *)
let s_of_codevec x =
  let xs = Array.to_list x in
  let v = Ocaml.vof_list Meta_opcode.vof_opcode xs in
  Ocaml.string_of_v v
(*e: function [[Dumper_.s_of_codevec]] *)
(*e: shell/Dumper_.ml *)
