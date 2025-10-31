open Common
open Types
open Pagetable_

type t = Pagetable_.t

let alloc () =
  { pagetab = Array.make pagetab_size None;
    first = pagetab_size; (* take care of access outside bound *)
    last = 0;
  }

(* less: pass seg type if want to handle SG_PHYSICAL (or handle it in caller)*)
let free pt =
  if pt.first < pagetab_size
  then 
    for i = pt.first to pt.last do
      pt.pagetab.(i) |> Option.iter Page.free;
    done

(* actually share the pages, but create a fresh pagetable so when fault,
 * we will be able to allocate a new page (copy on write) and reference
 * this new page in the pagetab array.
 *)
let copy pt_old =
  let pt_new = alloc () in
  pt_new.first <- pt_old.first;
  pt_new.last <- pt_old.last;
  if pt_new.first < pagetab_size
  then begin
    for i = pt_new.first to pt_new.last do
      pt_old.pagetab.(i) |> Option.iter (fun p -> 
        pt_new.pagetab.(i) <- Some (Page.share p);
      );
    done
  end;
  pt_new

