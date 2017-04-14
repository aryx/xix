open Types

(* use up 
*)
let ok_addr_range addr len write =
  (* less: every sections are writable for now. Just need
   * extra check and a Segment_.readonly for Kimage?
   *)
  (* todo: use Proc_segment.segment_of_addr *)
  raise Todo

let valid_addr_range addr len write =
  raise Todo
