open Common
open Types
open User_memory (* for operators @<, @-, etc *)

let change_segment_top addr section =
  let up = Globals.up () in
  let seg =
    try Hashtbl.find up.Proc_.seg section
    with Not_found -> Error.error Error.Ebadarg
  in
  (* less: why need lock? who else will modify up.seg? the pager? *)
  seg.Segment_.ql |> Qlock.with_lock  (fun () ->
    let new_top = User_memory.roundup_page addr in
    if new_top @< seg.Segment_.top
    then raise Todo
    else begin
      (* make sure new_top does not overlap with another segment *)
      up.Proc_.seg |> Hashtbl.iter (fun section2 seg2 ->
        if section2 <> section
        then
          if new_top >= seg2.Segment_.base && new_top < seg2.Segment_.top
          then Error.error Error.Esoverlap
      );

      let new_nb_pages = (new_top @- seg.Segment_.base) / Memory.pg2by in
      (* code similar in Segment.alloc *)
      if new_nb_pages > Segment_.pagedir_size * Pagetable_.pagetab_size
      then Error.error Error.Enovmem;

      let new_pgdir_size = 
        Int_.roundup new_nb_pages Pagetable_.pagetab_size 
        / Pagetable_.pagetab_size
      in
      let old_pgdir_size = Array.length seg.Segment_.pagedir in
      if new_pgdir_size > old_pgdir_size
      then begin
        let newarr = Array.make new_pgdir_size None in
        Array.blit seg.Segment_.pagedir 0 newarr 0 old_pgdir_size;
        seg.Segment_.pagedir <- newarr;
      end;
      seg.Segment_.top <- new_top;
      seg.Segment_.nb_pages <- new_nb_pages;
    end
  )

(* brk means?  *)
let syscall_brk addr =
  (* less: allow addr_opt and None where return base? useful? *)
  change_segment_top addr Proc_.SBss
