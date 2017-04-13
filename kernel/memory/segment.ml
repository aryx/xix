open Common
open Types

type kind = 
  | SText
  | SData
  | SBss
  | SStack

type t = {
  (* todo: why it matters here? Dupe with Proc.segtype? *)
  kind: kind;
  
  base: user_addr;
  top: user_addr;

  pagedir: Pagetable.t option array; (* length = pagedir_size = 1984 | 16 *)

  nb_pages: int;

  (* use for reference count and for its lock *)
  l: Ref.t;
  ql: Qlock.t;
}

(* less: use growing array? *)
let pagedir_size = 1024 (*todo: 1984 *)
(* let pagedir_size_small = 16 *)


let alloc kind base nb_pages =
  if nb_pages > pagedir_size * Pagetable.pagetab_size
  then failwith "Enovmem";
  let top = match base with VU x -> VU (x + nb_pages * Memory.pg2by) in
  let pgdir_size = 
    Common.round_up nb_pages Pagetable.pagetab_size / Pagetable.pagetab_size
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
  offset / Pagetable.pagetab_memory_mapped
let ptx offset = 
  (offset land (Pagetable.pagetab_memory_mapped - 1)) / Memory.pg2by
  

let add_page_to_segment page seg =
  let (VU va) = page.Page.va in
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
  if pt.Pagetable.pagetab.(ptx offset) <> None
  then failwith "add_page_to_segment: address already mapped to a page";
  pt.Pagetable.pagetab.(ptx offset) <- Some page;
  
  let i = ptx offset in
  if i < pt.Pagetable.first
  then pt.Pagetable.first <- i;
  if i > pt.Pagetable.last
  then pt.Pagetable.last <- i;

  ()
