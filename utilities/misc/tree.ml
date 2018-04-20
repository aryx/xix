(* src:
 *http://blog.shaynefletcher.org/2017/10/how-to-render-trees-like-unix-tree.html
 *)

(* A type of non-empty trees of strings. *)
type tree = [
  |`Node of string * tree list
]
;;

(* [print_tree tree] prints a rendering of [tree]. *)
let rec print_tree
          ?(pad : (string * string)= ("", ""))
          (tree : tree) : unit =
  let pd, pc = pad in
  match tree with
  | `Node (tag, cs) ->
     Printf.printf "%s%s\n" pd tag;
     let n = List.length cs - 1 in
     List.iteri (
         fun i c ->
         let pad =
           (pc ^ (if i = n then "`-- " else "|-- "),
            pc ^ (if i = n then "    " else "|   ")) in
         print_tree ~pad c
       ) cs
;;

(* An example tree. *)
let tree =
  `Node ("."
        , [
            `Node ("S", [
                      `Node ("T", [
                                `Node ("U", [])]);
                      `Node ("V", [])])
          ;  `Node ("W", [])
          ])
;;

(* Print the example tree. *)
let () =  print_tree tree
;;
The output of the above looks like this:

.
|-- S
|   |-- T
|   |   `-- U
|   `-- V
`-- W
