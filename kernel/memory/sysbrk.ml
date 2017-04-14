open Common
open Types

let change_segment_top addr section =
  let seg =
    try Hashtbl.find !(Globals.up).Proc_.seg section
    with Not_found -> failwith "change_segment_top: segment not found"
  in
  raise Todo

(* brk? *)
let syscall_brk addr_opt =
  match addr_opt with
  | None -> raise Todo
  | Some addr -> change_segment_top addr Proc_.SBss
