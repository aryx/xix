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
