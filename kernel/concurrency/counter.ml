open Common
open Types

(* less: delete once have { Xxx. ... } feature *)
open Ref_

type t = Ref.t

let gen x =
  Ref.inc x

let make () =
  { Ref_.
    cnt = 0;
    l = Spinlock.alloc ();
  }
