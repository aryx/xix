open Types

type kind = 
  | SText (* todo: of ?? *)
  | SData
  | SBss
  | SStack

type t = {
  (* Dupe with Proc.segtype? We need the kind here because
   * Segment.copy will do different things depending on the segment kind.
   *)
  kind: kind;
  (* less: read_only: bool; for KImage? *)
  
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
