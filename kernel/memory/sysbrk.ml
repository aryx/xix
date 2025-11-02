open Common

open Types
open User_memory.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let change_segment_top (addr : user_addr) (section : Process_.section) =
  let up : Process_.t = Globals.up () in
  let seg : Segment.t =
    try Hashtbl.find up.seg section
    with Not_found -> Error.error Error.Ebadarg
  in
  (* less: why need lock? who else will modify up.seg? the pager? *)
  seg.ql |> Qlock.with_lock  (fun () ->
    let new_top = User_memory.roundup_page addr in
    if new_top @< seg.top
    then raise Todo
    else begin
      (* make sure new_top does not overlap with another segment *)
      up.seg |> Hashtbl.iter (fun section2 (seg2 : Segment.t) ->
        if section2 <> section
        then
          if new_top >= seg2.base && new_top < seg2.top
          then Error.error Error.Esoverlap
      );

      let new_nb_pages = (new_top @- seg.base) / Memory.pg2by in
      (* code similar in Segment.alloc *)
      if new_nb_pages > Segment_.pagedir_size * Pagetable_.pagetab_size
      then Error.error Error.Enovmem;

      let new_pgdir_size = 
        Int_.roundup new_nb_pages Pagetable_.pagetab_size 
        / Pagetable_.pagetab_size
      in
      let old_pgdir_size = Array.length seg.pagedir in
      if new_pgdir_size > old_pgdir_size
      then begin
        let newarr = Array.make new_pgdir_size None in
        Array.blit seg.pagedir 0 newarr 0 old_pgdir_size;
        seg.pagedir <- newarr;
      end;
      seg.top <- new_top;
      seg.nb_pages <- new_nb_pages;
    end
  )

(*****************************************************************************)
(* The syscall *)
(*****************************************************************************)

(* brk means?  *)
let syscall_brk (addr : user_addr) : unit =
  (* less: allow addr_opt and None where return base? useful? *)
  change_segment_top addr Process_.SBss
