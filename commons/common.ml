
type filename = string

let spf = Printf.sprintf

type ('a, 'b) either = Left of 'a | Right of 'b

let with_file_out f file = 
  let chan = open_out file in
  let res = f chan in
  close_out chan;
  res

(*
=~
matched1
push
*)
