
type t = Page_.t

val alloc: Types.user_addr -> bool (* clear *) -> t
val free: t -> unit

val share: t -> t

type allocator = {

  (* big array! will map all pages from physical memory *)
  mutable pages: t array;

  (* less: use Queue.t instead? or Double_list.ml? *)
  mutable free: t list;
  mutable freecnt: int;

  (* !lock ordering! allocator.l before page.l *)
  l: Spinlock_.t;
}

val allocator : allocator
