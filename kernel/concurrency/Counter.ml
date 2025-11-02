open Common

open Types

type t = Ref.t

let alloc (n : int) : t = 
  Ref.alloc n

(* will start at n+1 when n is the count given to alloc *)
let gen (x : t) : int =
  Ref.inc x
