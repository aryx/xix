open Common
open Types

type t = {

  (* big array! *)
  mutable pages: Page.t array;

  (* less: use Queue.t instead? *)
  mutable free: Page.t list;
  mutable freecnt: int;

  (* !lock ordering! Page_allocator.l before Page.l *)
  l: Spinlock.t;
}

let allocator = {
  (* set in xinit *)
  pages = [| |];
  free = [];
  freecnt = 0;
  l = Spinlock.make ();
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
  

(* todo: should be swapalloc.highwater *)
let highwater = 100


let newpage clear segopt va =
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

