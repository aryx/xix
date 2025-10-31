open Types
  
(* The type below is different from Proc.section. We need to remember the
 * 'kind' of a segment in the segment because Segment.copy() will do 
 * different things depending on the kind (same for fix_fault()).
 * 
 * less: maybe we can remove 'kind' if we inline the decision upwards and 
 *  if Proc_segment.segment_of_addr return a pair Segment * Proc_.segtype
 *  (or need anyway two types because of the TSEG of SG_DATA?)
 *)
type kind = 
  | SText (* todo: of daddr ?? of Kimage?? *)
  | SData (* todo: of daddr ?? *)
  | SBss
  | SStack

type t = {
  kind: kind;
  (* less: read_only: bool; for KImage? *)
  
  base: user_addr;
  (* mutable because can be changed by sysbrk() (or segsysbrk()) *)
  mutable top: user_addr;

  (* Why not use 'Page_.t option array' directly? because we will
   * use page fault to populate this array, so this array will be sparse.
   * It is better to save space by going through a pagedir/pagatab division.
   * This division allows to implement efficiently a sparse array
   * (an alternative is (int, Page_.t) Hashtbl.t).
   * less: opti: todo? impose length = pagedir_size = 1984 | 16 
  *)
  mutable pagedir: Pagetable_.t option array; 
  mutable nb_pages: int;

  (* use for reference count and for its lock *)
  refcnt: Ref_.t;

  (* todo: when do we use this one? Need again because of pager? like
   * Proc_.seglock? Can't just use refcnt lock for that too?
   *)
  ql: Qlock_.t;
}

(* Just used as a marker for now. I use a growing array. *)
let pagedir_size = 1024 (*todo: 1984 *)
(* less: opti: let pagedir_size_small = 16 *)
