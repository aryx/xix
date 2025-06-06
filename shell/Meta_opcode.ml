(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_vof.cmo  pr_o.cmo /tmp/xxx.ml  *)

open Opcode

module Ocaml = OCaml

let vof_operation =
  function
  | Mark -> Ocaml.VSum (("Mark", []))
  | Word -> Ocaml.VSum (("Word", []))
  | Popm -> Ocaml.VSum (("Popm", []))
  | Count -> Ocaml.VSum (("Count", []))
  | Concatenate -> Ocaml.VSum (("Concatenate", []))
  | Stringify -> Ocaml.VSum (("Stringify", []))
  | Glob -> Ocaml.VSum (("Glob", []))
  | Assign -> Ocaml.VSum (("Assign", []))
  | Dollar -> Ocaml.VSum (("Dollar", []))
  | Index -> Ocaml.VSum (("Index", []))
  | Local -> Ocaml.VSum (("Local", []))
  | Unlocal -> Ocaml.VSum (("Unlocal", []))
  | Fn -> Ocaml.VSum (("Fn", []))
  | DelFn -> Ocaml.VSum (("DelFn", []))
  | Simple -> Ocaml.VSum (("Simple", []))
  | Exit -> Ocaml.VSum (("Exit", []))
  | Return -> Ocaml.VSum (("Return", []))
  | If -> Ocaml.VSum (("If", []))
  | IfNot -> Ocaml.VSum (("IfNot", []))
  | Jump -> Ocaml.VSum (("Jump", []))
  | Match -> Ocaml.VSum (("Match", []))
  | Case -> Ocaml.VSum (("Case", []))
  | For -> Ocaml.VSum (("For", []))
  | Wastrue -> Ocaml.VSum (("Wastrue", []))
  | Not -> Ocaml.VSum (("Not", []))
  | False -> Ocaml.VSum ((" False", []))
  | True -> Ocaml.VSum ((" True", []))
  | Read -> Ocaml.VSum (("Read", []))
  | Write -> Ocaml.VSum (("Write", []))
  | ReadWrite -> Ocaml.VSum (("ReadWrite", []))
  | Append -> Ocaml.VSum (("Append", []))
  | Close -> Ocaml.VSum (("Close", []))
  | Dup -> Ocaml.VSum (("Dup", []))
  | Pipe -> Ocaml.VSum (("Pipe", []))
  | PipeWait -> Ocaml.VSum (("PipeWait", []))
  | PipeFd -> Ocaml.VSum (("PipeFd", []))
  | Eflag -> Ocaml.VSum (("Eflag", []))
  | Subshell -> Ocaml.VSum (("Subshell", []))
  | Backquote -> Ocaml.VSum (("Backquote", []))
  | Async -> Ocaml.VSum (("Async", []))
  | REPL -> Ocaml.VSum (("REPL", []))
  | Popredir -> Ocaml.VSum (("Popredir", []))
  
let vof_opcode =
  function
  | F v1 -> let v1 = vof_operation v1 in Ocaml.VSum (("F", [ v1 ]))
  | I v1 -> let v1 = Ocaml.vof_int v1 in Ocaml.VSum (("I", [ v1 ]))
  | S v1 -> let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("S", [ v1 ]))

