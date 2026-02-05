open Common

(* This is incomplete and used only for plan9 stuff for now (e.g., windows/) *)

(*
 * alternatives:
 *  - uchar.ml?
 *  - extlib/utf8.ml ?
 *  - batteries?
 *  - camomile?
 *  - buenzli lib? uusomething?
 *)

(* todo: use unicode! use Uchar.t? *)
type t = char

(* todo: should return partial bytes if not full runes *)
let bytes_to_runes str =
  let res = ref [] in
  for i = 0 to String.length str -1 do
    res := str.[i] :: !res;
  done;
  List.rev !res


let string_of_runes xs =
  let str = Bytes.create (List.length xs) in
  xs |> List.iteri (fun i c ->
    Bytes.set str i c
  );
  Bytes.to_string str
