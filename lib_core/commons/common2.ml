open Stdcompat

let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)

let enum x n =
  if not(x <= n)
  then failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x = n then n::acc else enum_aux (x::acc) (x+1) n
  in
  List.rev (enum_aux [] x n)

let (list_of_string: string -> char list) = function
    "" -> []
  | s -> (enum 0 ((String.length s) - 1) |> List.map (String.get s))


let rec zip xs ys =
  match (xs,ys) with
  | ([],[]) -> []
  | ([],_) -> failwith "zip: not same length"
  | (_,[]) -> failwith "zip: not same length"
  | (x::xs,y::ys) -> (x,y)::zip xs ys

let index_list xs =
  if xs = [] 
  then [] (* enum 0 (-1) generate an exception *)
  else zip xs (enum 0 ((List.length xs) -1))

let index_list_1 xs =
  xs |> index_list |> List.map (fun (x,i) -> x, i+1)


let rec (span: ('a -> bool) -> 'a list -> 'a list * 'a list) =
 fun p -> function
  | []    -> ([], [])
  | x::xs ->
      if p x then
        let (l1, l2) = span p xs in
        (x::l1, l2)
      else ([], x::xs)
