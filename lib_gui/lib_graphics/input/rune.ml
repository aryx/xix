open Common

(*
 * alternatives:
 *  - extlib/utf8.ml ?
 *  - batteries?
 *  - camomile?
 *)

(* todo: use unicode! *)
type t = char

(* todo: should return partial bytes if not full runes *)
let bytes_to_runes str =
  let res = ref [] in
  for i = 0 to String.length str -1 do
    res := str.[i] :: !res;
  done;
  List.rev !res

let string_of_runes xs =
  let str = String.create (List.length xs) in
  xs |> List.iteri (fun i c ->
    str.[i] <- c
  );
  str
