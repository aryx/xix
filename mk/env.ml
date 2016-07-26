open Common

(* expanded variables and backquote *)
type values = string list

type t = {
  vars: (string, values) Hashtbl.t;
  internal_vars: (string, values) Hashtbl.t;
}

let internal_vars = [
  "target";
  "prereq";
  "stem";

  (* todo: alltargets, newprereq ... 
  *)
]

(* less: could take the readenv function as a parameter? *)
let initenv () =
  pr2 "Todo:initenv";
  { vars = Hashtbl.create 101;
    internal_vars = Hashtbl.create 101;
  }
