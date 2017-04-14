open Common
open Types
open Pagetable_

type t = Pagetable_.t

let alloc () =
  { pagetab = Array.make pagetab_size None;
    first = pagetab_size; (* take care of access outside bound *)
    last = 0;
  }

(* less: pass segment too? or just pass segment type? *)
let free pt =
  if pt.first < pagetab_size
  then 
    for i = pt.first to pt.last do
      pt.pagetab.(i) |> Common.if_some (fun p -> Page_allocator.free p);
    done

let copy pt_old =
  let pt_new = alloc () in
  pt_new.first <- pt_old.first;
  pt_new.last <- pt_old.last;
  if pt_new.first < pagetab_size
  then begin
    for i = pt_new.first to pt_new.last do
      pt_old.pagetab.(i) |> Common.if_some (fun p -> 
        Spinlock.lock p.Page.l;
        p.Page.refcnt <- p.Page.refcnt + 1;
        Spinlock.unlock p.Page.l;
        pt_new.pagetab.(i) <- Some p;
      );
    done
  end;
  pt_new

