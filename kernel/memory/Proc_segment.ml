open Types

(* less: mv in segment.ml? *)

exception Found of Segment_.t

(* todo: dolock bool (complex Segment_.ql locking) 
 * less: return option instead of Not_found?
 * todo: return section! so less need store segment kind?
 *  or pb when have STEXT in SG_DATA?
 *)
let segment_of_addr (p : Process_.t) (addr : Types.user_addr) : Segment.t =
  try 
    p.seg |> Hashtbl.iter (fun _section (seg : Segment_.t) ->
        let (VU va) = addr in
        let (VU base) = seg.base in
        let (VU top) = seg.top in
        if va >= base && va < top
        then raise (Found seg)
    );
    raise Not_found
  with Found x -> x
  
