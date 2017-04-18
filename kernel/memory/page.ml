open Common
open Types
open Page_

type t = Page_.t

(* We have to use an allocator here. We can not just use ocaml malloc
 * to create Page from thin air because a Page references a physical
 * memory range!
 *)
type allocator = {

  (* big array! will map all pages from physical memory *)
  mutable pages: t array;

  (* less: use Queue.t instead? or Double_list.ml? *)
  mutable free: t list;
  mutable freecnt: int;

  (* !lock ordering! allocator.l before page.l *)
  l: Spinlock.t;
}

let allocator = {
  (* set in init *)
  pages = [| |];

  free = [];
  freecnt = 0;
  l = Spinlock.alloc ();
}

let unchain p =
  if Spinlock.canlock allocator.l
  then failwith "unchain: palloc lock should be held";

  match allocator.free with
  | [] -> raise (Impossible "unchain should be called only when found a free")
  | x::xs ->
    if x != p
    then raise (Impossible "unchain should be called with top free page");
    allocator.free <- xs;
    allocator.freecnt <- allocator.freecnt - 1;
    ()

let chain_free_head p =
  if Spinlock.canlock allocator.l
  then failwith "chain_free_head: palloc lock should be held";
  allocator.free <- p::allocator.free;
  allocator.freecnt <- allocator.freecnt + 1;
  ()

(* todo: when need that? *)
let chain_free_tail p =
  raise Todo

(* todo: should be swapalloc.highwater *)
let highwater = 100


let alloc va clear (* less: segopt *) =
  Spinlock.lock allocator.l;

  if allocator.freecnt > highwater
  then begin
    let p = List.hd allocator.free in
    unchain p;
    (* less: why lock? we should be the only one anyway no? for consistency? *)
    Spinlock.lock p.Page_.l;
    if p.refcnt <> 0
    then failwith "newpage: page ref != 0";
    (* less: uncachepage *)
    p.refcnt <- 1;
    p.va <- va;
    p.modified <- false;
    p.referenced <- false;
    Spinlock.unlock p.Page_.l;
    Spinlock.unlock allocator.l;

    if clear 
    then begin
      let ka = Kernel_memory.kmap p in
      Kernel_memory.memclear ka Memory.pg2by;
      Kernel_memory.kunmap ka
    end;
    p
  end
  else 
    failwith "TODO: very few free pages"

(* less: if page is a swapaddress? *)
let free p =
  Spinlock.lock allocator.l;
  (* we should use Ref.dec_and_is_zero but for opti reason we
   * use a spinlock and a separate int for a refcnt (and not a Ref)
   * so have to be more verbose
   *)
  Spinlock.lock p.Page_.l;
  if p.refcnt = 0
  then failwith "Page.free: refcnt is 0";
  p.refcnt <- p.refcnt - 1;
  if p.refcnt > 0
  then begin
    Spinlock.unlock p.Page_.l;
    Spinlock.unlock allocator.l;
  end else begin
    chain_free_head p;
    (* todo: wakeup if people wait on freememr *)
    Spinlock.unlock p.Page_.l;
    Spinlock.unlock allocator.l;
  end


(* init the allocator *)
let init_allocator xs =
  raise Todo


let share p = 
  Spinlock.lock p.Page_.l;
  p.refcnt <- p.refcnt + 1;
  Spinlock.unlock p.Page_.l;
  p
