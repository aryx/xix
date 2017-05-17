open Common

module Unix2 = ThreadUnix

type fcall = {
  fid: int;
}

let read fd buf =
  
  let n = Unix2.read fd buf 0 (String.length buf) in
  failwith "ninep.read: Todo"

(* was called convM2S *)
let parse buf =
  raise Todo

