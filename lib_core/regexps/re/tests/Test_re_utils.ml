open Either

(*****************************************************************************)
(* Fort to Testo adapter *)
(*****************************************************************************)

let expect_pass = Testo.create

let try_with f = try Right (f ()) with exn -> Left exn

let expect_equal_app ?printer ?msg f x g y =
  let fx = try_with (fun () -> f x) in
  let gy = try_with (fun () -> g y) in
  match fx, gy with
  | Right fx, Right gy -> OUnit.assert_equal ?printer ?msg fx gy
  | Left e1, Left e2 -> OUnit.assert_equal ~printer:(Printexc.to_string) ?msg e1 e2
  | Left _, Right _ -> OUnit.assert_failure "f x raised while g y didn't"
  | Right _, Left _ -> OUnit.assert_failure "g y raised while f x didn't"

(*****************************************************************************)
(* Was in ocaml-re/lib_test/env.ml *)
(*****************************************************************************)
(* open Fort *)

let id x = x
let not_found () = raise Not_found

let str_printer s = "\"" ^ String.escaped s ^ "\""
let ofs_printer (i0,i1) = Printf.sprintf "(%d,%d)" i0 i1
let list_printer f l =
   "[" ^ (String.concat "; " (List.map f l)) ^ "]"
let arr_printer f a =
   "[|" ^ (String.concat "; " (List.map f (Array.to_list a))) ^ "|]"

let arr_str_printer = arr_printer str_printer
let arr_ofs_printer = arr_printer ofs_printer

(*
let expect_eq_bool      = expect_equal_app ~printer:string_of_bool
let expect_eq_str       = expect_equal_app ~printer:str_printer
let expect_eq_ofs       = expect_equal_app ~printer:ofs_printer
let expect_eq_arr_str   = expect_equal_app ~printer:arr_str_printer
let expect_eq_arr_ofs   = expect_equal_app ~printer:arr_ofs_printer
*)

let expect_eq_bool ?msg f x g y =
  expect_equal_app ?msg ~printer:string_of_bool f x g y
let expect_eq_str ?msg f x g y =
  expect_equal_app ?msg ~printer:str_printer f x g y
let expect_eq_ofs ?msg f x g y =
  expect_equal_app ?msg ~printer:ofs_printer f x g y
let expect_eq_arr_str ?msg f x g y =
  expect_equal_app ?msg ~printer:arr_str_printer f x g y
let expect_eq_arr_ofs  ?msg f x g y =
  expect_equal_app ?msg ~printer:arr_ofs_printer f x g y

