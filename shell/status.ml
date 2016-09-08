open Common

module R = Runtime

let setstatus s =
  Var.setvar "status" [s]

let getstatus () =
  let v = (Var.vlook "status").R.v in
  match v with
  | None -> ""
  | Some [x] -> x
  (* stricter: should never happen *)
  | Some _ -> failwith "getstatus: $status is a list with more than one element"

let concstatus s1 s2 =
  s1 ^ "|" ^ s2
