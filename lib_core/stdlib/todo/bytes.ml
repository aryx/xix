(* partial port of 4.02 bytes.ml just enough to compile some of ocaml-light with
 * dune and a recent OCaml (as well as xix)
 *)
type t = string

let empty = ""

let sub x n1 n2 = String.sub x n1 n2

let sub_string x n1 n2 = String.sub x n1 n2

let length x = String.length x

let create x = String.create x

let to_string x = x

let of_string x = x

let get x n = String.get x n

let set s x c = String.set s x c

let index_from s n c = String.index_from s n c

let unsafe_get x n = String.unsafe_get x n

let unsafe_to_string x = x

let unsafe_of_string x = x

let blit src srcoff dst dstoff len =
  String.blit src srcoff dst dstoff len

let unsafe_blit src srcoff dst dstoff len =
  String.unsafe_blit src srcoff dst dstoff len
