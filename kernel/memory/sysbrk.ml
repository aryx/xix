open Common
open Types
open User_memory (* for operators @<, @-, etc *)

let change_segment_top addr section =
  let seg =
    try Hashtbl.find !(Globals.up).Proc_.seg section
    with Not_found -> raise Error.Ebadarg
  in
  seg.Segment_.ql |> Qlock.with_lock  (fun () ->
    let new_top = Memory.roundup_page addr in
    if new_top @< seg.Segment_.top
    then raise Todo
    else begin
      (* make sure new_top does not overlap with another segment *)
      !(Globals.up).Proc_.seg |> Hashtbl.iter (fun section2 seg2 ->
        if section2 <> section
        then
          if new_top >= seg2.Segment_.base && new_top < seg2.Segment_.top
          then raise Error.Esoverlap
      );

      let new_nb_pages = (new_top @- seg.Segment_.base) / Memory.pg2by in
      (* code similar in Segment.alloc *)
      if new_nb_pages > Segment_.pagedir_size * Pagetable_.pagetab_size
      then raise Error.Enovmem;

      let new_pgdir_size = 
        Common.roundup new_nb_pages Pagetable_.pagetab_size 
        / Pagetable_.pagetab_size
      in
      let old_pgdir_size = Array.length seg.Segment_.pagedir in
      if new_pgdir_size > old_pgdir_size
      then begin
        let newarr = Array.make new_pgdir_size None in
        Array.blit seg.Segment_.pagedir 0 newarr 0 old_pgdir_size;
      end;
      seg.Segment_.top <- new_top;
      seg.Segment_.nb_pages <- new_nb_pages;
    end
  )

(* brk? *)
let syscall_brk addr_opt =
  match addr_opt with
  | None -> raise Todo
  | Some addr -> change_segment_top addr Proc_.SBss
