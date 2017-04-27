open Common

exception Exn1 of string
exception Exn2 of string

let bar x =
  if true
  then raise (Exn1 "bar")
  else 1

let foo x = 
  bar x

let test_print () =
  let x = 1+1 in
  let s = Printf.sprintf "hello world %d\n" x in
  print_string s;
  (try 
    let x = foo 42 in
    print_string (Printf.sprintf "res = %d" x);
  with Exn1 s ->
    print_string (Printf.sprintf "exn1 = %s" s);
  )

let test_threads () =
  let t1 = Thread.create (fun () ->
    print_string "thread 1\n";
  ) () in
  let t2 = Thread.create (fun () ->
    print_string "thread 2\n";
  ) () in
  Thread.sleep ();

  while true do 
    for i = 0 to 1000 do
      ()
    done
  done



let test () =
  (* test_print () *)
  test_threads ();
  ()
