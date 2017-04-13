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


let newpage clear segopt va =
  raise Todo
