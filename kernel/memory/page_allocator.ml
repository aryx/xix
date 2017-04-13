open Common
open Types

type t = {

  (* big array! *)
  mutable pages: Page.t array;

  (* less: use Queue.t instead? or Double_list.ml? *)
  mutable free: Page.t list;
  mutable freecnt: int;

  (* !lock ordering! Page_allocator.l before Page.l *)
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
    allocator.freecnt <- allocator.freecnt -1;
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


let alloc clear segopt va =
  Spinlock.lock allocator.l;

  if allocator.freecnt > highwater
  then begin
    let p = List.hd allocator.free in
    unchain p;
    Spinlock.lock p.Page.l;
    if p.Page.refcnt <> 0
    then failwith "newpage: page ref != 0";
    (* less: uncachepage *)
    p.Page.refcnt <- 1;
    p.Page.va <- va;
    p.Page.modified <- false;
    p.Page.referenced <- false;
    Spinlock.unlock p.Page.l;
    Spinlock.unlock allocator.l;

    if clear 
    then Memory.memclear va Memory.pg2by;
    p
  end
  else 
    failwith "TODO: very few free pages"

(* less: if page is a swapaddress? *)
let free p =
  Spinlock.lock allocator.l;
  Spinlock.lock p.Page.l;
  if p.Page.refcnt = 0
  then failwith "Page_allocator.free: refcnt is 0";
  p.Page.refcnt <- p.Page.refcnt - 1;
  if p.Page.refcnt > 0
  then begin
    Spinlock.unlock p.Page.l;
    Spinlock.unlock allocator.l;
  end else begin
    chain_free_head p;
    (* todo: wakeup if people wait on freememr *)
    Spinlock.unlock p.Page.l;
    Spinlock.unlock allocator.l;
  end



let init xs =
  raise Todo
