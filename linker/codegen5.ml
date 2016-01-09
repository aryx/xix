open Common

(* 
 * No need for optab/oplook/ocmp/cmp. Just use pattern matching!
 * (but we might not win or even lose on the bit manip stuff)
 *)

type operand_class = Xxx

let immrot x =
  raise Todo

let immaddr x =
  raise Todo


let gen_one cg instr =
  raise Todo

let size_of_instruction symbols2 node =
  raise Todo

let gen symbols cg =
  raise Todo

(* double check pc is like one computed by layout_text 
 otherwise failwith  "phase error ..."
*)

