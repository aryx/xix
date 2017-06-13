
(* todo: use unicode! *)
type t = char

(* todo: should return partial bytes if not full runes *)
let bytes_to_runes str =
  let res = ref [] in
  for i = 0 to String.length str -1 do
    res := str.[i] :: !res;
  done;
  List.rev !res
