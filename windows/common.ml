(* available since ocaml 4.01 but we use 1.07 (ocaml light) for rio *)
let (|>) o f = f o

let spf = Printf.sprintf

let if_some f = function
  | None -> ()
  | Some x -> f x

exception Todo
exception Impossible of string
