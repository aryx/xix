open Common
open Types

(* less: put in core/ too? or no mutual deps for Rwlock.t? *)
type t = {
  (* instead of a 'locked: bool' *)
  mutable readers: int;
  mutable writer: bool;

  (* the readers *)
  rprocs: Proc_.t Queue.t;
  (* the writer *)
  mutable wproc: Proc_.t option;

  l: Spinlock.t;
}

let rlock x =
  raise Todo

let runlock x =
  raise Todo

let wlock x =
  raise Todo

let wunlock x =
  raise Todo


let canrlock x =
  raise Todo
