
let test_print () =
  let x = 1+1 in
  let s = Printf.sprintf "hello world %d\n" x in
  print_string s


let test () =
(*
  test_print ();
*)
  test_print ();
  ()
