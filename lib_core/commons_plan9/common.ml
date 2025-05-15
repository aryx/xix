(* TODO: reuse plan9-ml/commons/common.ml at some point *)

(* todo: constructor to sanity check *)
type int8 = int
(* todo: constructor to sanity check *)
type int16 = int

(* todo: use Int32.t, builtin since OCaml ?? *)
type int32 = int
(* todo: use Int64.t, builtin since OCaml ?? *)
type int64 = int
(* if you really need to represent high values use: 
 * type int64_special = int * int 
 *)

type byte = char

(* builtin since OCaml 4.02 *)
type bytes = string

type filename = string

type ('a, 'b) either = Left of 'a | Right of 'b

exception Todo
exception Impossible of string

(* builtin since ocaml 4.01 (and optimized) *)
let (|>) o f = f o

let spf = Printf.sprintf

let pr s =
  print_string (s ^ "\n");
  flush stdout

let if_some f = function
  | None -> ()
  | Some x -> f x

(* weird: if add this then get some suicide on hellorio/rio/etc
let some = function
  | None -> raise (Impossible "some: should have a Some here")
  | Some x -> x
*)

let once aref f =
  match !aref with
  | Some x -> x
  | None ->
    let x = f () in
    aref := Some x;
    x

module Regexp = struct

let (matched: int -> string -> string) = fun i s ->
  Str.matched_group i s

let matched1 = fun s -> matched 1 s
let matched2 = fun s -> (matched 1 s, matched 2 s)
let matched3 = fun s -> (matched 1 s, matched 2 s, matched 3 s)
let matched4 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
let matched5 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
let matched6 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)
let matched7 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s, matched 7 s)

(*
let _memo_compiled_regexp = Hashtbl.create 101
let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0
*)
let split sep s = Str.split (Str.regexp sep) s
end

let (=~) s re =
  Str.string_match (Str.regexp re) s 0
  (*candidate_match_func s re*)


module Obj_ = struct

(*
(* start of dumper.ml *)

(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * dumper.ml 1.2 2005/02/06 12:38:21 rich Exp
 *)
open Printf
open Obj

let rec dump2 r =
  if is_int r then
    string_of_int (magic r : int)
  else (				(* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (field r n :: acc) n
    in
    let rec is_list r =
      if is_int r then (
        if (magic r : int) = 0 then true (* [] *)
        else false
      ) else (
        let s = size r and t = tag r in
        if t = 0 && s = 2 then is_list (field r 1) (* h :: t *)
        else false
      )
    in
    let rec get_list r =
      if is_int r then []
      else let h = field r 0 and t = get_list (field r 1) in h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible in
       * pure OCaml at the moment.
       *)
      "<" ^ name ^ ">"
    in

    let s = size r and t = tag r in

    (* From the tag, determine the type of block. *)
    if is_list r then ( (* List. *)
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump2 fields) ^ "]"
    )
    else if t = 0 then (		(* Tuple, array, record. *)
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )

    (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
     * clear if very large constructed values could have the same
     * tag. XXX *)
    else if t = lazy_tag then opaque "lazy"
    else if t = closure_tag then opaque "closure"
    else if t = object_tag then (	(* Object. *)
      let fields = get_fields [] s in
      let clasz, id, slots =
        match fields with h::h'::t -> h, h', t | _ -> assert false in
      (* No information on decoding the class (first field).  So just print
       * out the ID and the slots.
       *)
      "Object #" ^ dump2 id ^
        " (" ^ String.concat ", " (List.map dump2 slots) ^ ")"
    )
    else if t = infix_tag then opaque "infix"
    else if t = forward_tag then opaque "forward"

    else if t < no_scan_tag then (	(* Constructed value. *)
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
        " (" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )
    else if t = string_tag then (
      "\"" ^ String.escaped (magic r : string) ^ "\""
    )
    else if t = double_tag then (
      string_of_float (magic r : float)
    )
    else if t = abstract_tag then opaque "abstract"
    else if t = custom_tag then opaque "custom"
    else if t = final_tag then opaque "final"
    else failwith ("dump: impossible tag (" ^ string_of_int t ^ ")")
  )

let dump v = dump2 (repr v)

(* end of dumper.ml *)
*)
end

module List_ = struct

(* todo: remove once get List.iteri in 1.07 *)
let iteri f xs =
  xs |> Array.of_list |> Array.iteri f
end

module Hashtbl_ = struct

let to_list h =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) h []

end

(* tail recursive efficient version *)
(* TODO: seems to not work when reading /dev/winname in test_rio_graph_app1 *)
let cat file =
  let chan = open_in file in
  let rec cat_aux acc ()  =
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () |> List.rev |> (fun x -> close_in chan; x)
