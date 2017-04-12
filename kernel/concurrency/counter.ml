open Common
open Types

(* less: delete once have { Xxx. ... } feature *)
open Ref

type t = Ref.t

let gen x =
  Ref.inc x

let make () =
  { Ref.
    cnt = 0;
    l = Spinlock.make ();
  }
