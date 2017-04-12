open Common
open Types

(* todo:
 * - use monitor approach instead of fine-grained locks?
 *)

type t = {
  mutable readers: int;
  mutable writer: bool;

  q: Proc.t Queue.t;
  mutable wproc: Proc.t option;

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
