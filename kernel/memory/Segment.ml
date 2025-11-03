open Common
open Types
open Segment_

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Segment_.t
type kind = Segment_.kind

(*****************************************************************************)
(* alloc/free *)
(*****************************************************************************)

let alloc (kind : kind) (base : Types.user_addr) (nb_pages : int) : t =
  if nb_pages > Segment_.pagedir_size * Pagetable_.pagetab_size
  then Error.error Error.Enovmem;

  let top = match base with VU x -> VU (x + nb_pages * Memory.pg2by) in
  let pgdir_size = 
    (* each pgdir entry will have pagetab_size pgtab entries *)
    Int_.roundup nb_pages Pagetable_.pagetab_size / Pagetable_.pagetab_size
  in
  { 
    kind = kind;
    base = base;
    top = top;
    nb_pages = nb_pages;
    pagedir = Array.make pgdir_size None;
    refcnt = Ref.alloc 1;
    ql = Qlock.alloc ();
  }
  
let free (seg : t) : unit =
  if Ref.dec_and_is_zero seg.refcnt
  then begin
    Qlock.lock seg.ql;
    seg.pagedir |> Array.iter (fun pagedir ->
      pagedir |> Option.iter (fun pagetable ->
        Pagetable.free pagetable;
      );
    );
    Qlock.unlock seg.ql;
  end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pdx offset = 
  offset / Pagetable_.pagetab_memory_mapped
let ptx offset = 
  (offset land (Pagetable_.pagetab_memory_mapped - 1)) / Memory.pg2by
  

(* less: no seglock? *)
let _add_page_to_segment (page : Page_.t) (seg : t) : unit =
  let (VU va) = page.va in
  let (VU base) = seg.base in
  let (VU top) = seg.top in
  
  if va < base || va >= top
  then failwith "add_page_to_segment: page out of segment range";
  
  let offset = va - base in
  let pt : Pagetable_.t =
    match seg.pagedir.(pdx offset) with
    | None -> 
      let pt = Pagetable.alloc () in
      seg.pagedir.(pdx offset) <- Some pt;
      pt
    | Some x -> x
  in
  if pt.pagetab.(ptx offset) <> None
  then failwith "add_page_to_segment: address already mapped to a page";

  pt.pagetab.(ptx offset) <- Some page;

  let i = ptx offset in
  if i < pt.first
  then pt.first <- i;
  if i > pt.last
  then pt.last <- i;
  ()



let really_share seg =
  Ref.inc seg.refcnt |> ignore;
  seg

let really_copy oldseg =
  let seg = alloc oldseg.kind oldseg.base oldseg.nb_pages in
  oldseg.pagedir |> Array.iteri (fun i x ->
    x |> Option.iter (fun pt -> 
      seg.pagedir.(i) <- Some (Pagetable.copy pt)
  )
  );
  seg

(* less: pass also proc segment type, to handle KImage opti and data2txt *)
let copy_or_share seg share =
  seg.ql |> Qlock.with_lock (fun () ->
    match seg.kind with
    (* always share *)
    | SText -> really_share seg
    (* always new *)
    | SStack -> really_copy seg
    (* share or copy *)
    | SData | SBss ->
      (* less: if SData and Proc_SText? data2txt *)
      if share
      then really_share seg
      else really_copy seg
  )
  

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let share (seg : t) : t =
  copy_or_share seg true

let copy (seg : t) : t =
  copy_or_share seg false
