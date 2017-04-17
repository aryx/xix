open Types
  
(* This is different than Proc.section. We need to remember a kind
 * in the segment because Segment.copy will do different things 
 * depending on the kind (and fix_fault).
 * todo: maybe can remove it if inline the decision upwards and 
 *  if Proc_segment.segment_of_addr return a pair Segment * Proc_.segtype
 *  or need anyway two types because of the TSEG of SG_DATA?
 *)
type kind = 
  | SText (* todo: of ?? *)
  | SData
  | SBss
  | SStack

type t = {
  kind: kind;
  (* less: read_only: bool; for KImage? *)
  
  base: user_addr;
  (* mutable because can be changed by sysbrk() *)
  mutable top: user_addr;

  (* less: opti: todo? impose length = pagedir_size = 1984 | 16 *)
  mutable pagedir: Pagetable_.t option array; 
  mutable nb_pages: int;

  (* use for reference count and for its lock *)
  refcnt: Ref_.t;
  ql: Qlock_.t;

}

(* Just use as a maker for now. I use a growing array. *)
let pagedir_size = 1024 (*todo: 1984 *)
(* less: opti: let pagedir_size_small = 16 *)
