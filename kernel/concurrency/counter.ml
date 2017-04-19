open Common
open Types

type t = Ref.t

(* will start at n+1 when n is the count given to alloc *)
let gen x =
  Ref.inc x

(* less: delete once have { Xxx.xxx= yyy= ... } feature *)
open Ref_

let alloc n = 
  Ref.alloc n
