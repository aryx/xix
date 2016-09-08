open Common

module R = Runtime

let setstatus s =
  Var.setvar "status" [s]


