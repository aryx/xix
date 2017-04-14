open Common
open Types
open Segment_

type t = Segment_.t

let alloc kind base nb_pages =
  if nb_pages > Segment_.pagedir_size * Pagetable_.pagetab_size
  then raise Error.Enovmem;
  let top = match base with VU x -> VU (x + nb_pages * Memory.pg2by) in
  let pgdir_size = 
    Common.roundup nb_pages Pagetable_.pagetab_size / Pagetable_.pagetab_size
  in
  { 
    kind = kind;
    base = base;
    top = top;
    nb_pages = nb_pages;
    pagedir = Array.make pgdir_size None;
    l = Ref.alloc ();
    ql = Qlock.alloc ();
  }
  
let free seg =
  let cnt = Ref.dec seg.l in
  if cnt = 0
  then begin
    Qlock.lock seg.ql;
    seg.pagedir |> Array.iter (fun pagedir ->
      pagedir |> Common.if_some (fun pagetable ->
        Pagetable.free pagetable;
      );
    );
    Qlock.unlock seg.ql;
  end

let pdx offset = 
  offset / Pagetable_.pagetab_memory_mapped
let ptx offset = 
  (offset land (Pagetable_.pagetab_memory_mapped - 1)) / Memory.pg2by
  

let add_page_to_segment page seg =
  let (VU va) = page.Page_.va in
  let (VU base) = seg.base in
  let (VU top) = seg.top in
  
  if va < base || va >= top
  then failwith "add_patch_to_segment: page out of segment range";
  
  let offset = va - base in
  let pt =
    match seg.pagedir.(pdx offset) with
    | None -> 
      let pt = Pagetable.alloc () in
      seg.pagedir.(pdx offset) <- Some pt;
      pt
    | Some x -> x
  in
  if pt.Pagetable_.pagetab.(ptx offset) <> None
  then failwith "add_page_to_segment: address already mapped to a page";
  pt.Pagetable_.pagetab.(ptx offset) <- Some page;
  
  let i = ptx offset in
  if i < pt.Pagetable_.first
  then pt.Pagetable_.first <- i;
  if i > pt.Pagetable_.last
  then pt.Pagetable_.last <- i;

  ()
