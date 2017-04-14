open Types

type kind = 
  | SText
  | SData
  | SBss
  | SStack

type t = {
  (* todo: why it matters here? Dupe with Proc.segtype? *)
  kind: kind;
  
  base: user_addr;
  top: user_addr;

  pagedir: Pagetable_.t option array; (* length = pagedir_size = 1984 | 16 *)

  nb_pages: int;

  (* use for reference count and for its lock *)
  l: Ref_.t;
  ql: Qlock_.t;
}

(* less: use growing array? *)
let pagedir_size = 1024 (*todo: 1984 *)
(* let pagedir_size_small = 16 *)
