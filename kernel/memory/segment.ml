open Common
open Types

type kind = 
  | Text
  | Data
  | Bss
  | Stack

type t = {
  (* todo: why it matters here? Dupe with Proc.segtype? *)
  kind: kind;
  
  base: user_addr;
  top: user_addr;

  pagedir: Pagetable.t option array; (* length = pagedir_size = 1984 | 16 *)

  nb_pages: int;

  (* use for reference count and for its lock *)
  l: Ref.t;
  lk: Qlock.t;
}

(* less: use growing array? *)
let pagedir_size = 1024 (*todo: 1984 *)
let pagedir_size_small = 16


