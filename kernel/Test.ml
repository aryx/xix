open Common

let thread_wakeup _ = 
  let _ = failwith "TODO" in
  ()

let thread_sleep _ = 
  let _ = failwith "TODO" in
  ()

let test_print () =
  let x = 1+1 in
  let s = Printf.sprintf "hello world %d\n" x in
  print_string s


exception Exn1 of string
exception Exn2 of string
let bar _x =
  if true
  then raise (Exn1 "bar")
  else 1
let foo x = 
  bar x
let test_exn () =
  (try 
    let x = foo 42 in
    print_string (Printf.sprintf "res = %d" x);
  with Exn1 s ->
    print_string (Printf.sprintf "exn1 = %s" s);
  )

let test_threads_cooperatively () =
  let xt1 = ref None in
  let xt2 = ref None in

  let t1 = Thread.create (fun () ->
    print_string "thread 1\n";
    thread_sleep ();
    print_string "thread 1 bis\n";
    !xt2 |> Option.iter (fun t2 -> thread_wakeup t2);
  ) () in
  xt1 := Some t1;
  let t2 = Thread.create (fun () ->
    print_string "thread 2\n";
    !xt1 |> Option.iter (fun t1 -> thread_wakeup t1);
    thread_sleep ();
    print_string "thread 2 bis\n";
  ) () in
  xt2 := Some t2;
  thread_sleep ()


let test_threads_preemptively () =

  let loop () = 
    while true do 
      for _i = 0 to 1000 do
        ()
      done
    done
  in

  let _t1 = Thread.create (fun () ->
    print_string "thread 1\n";
    loop ()
  ) () in
  let _t2 = Thread.create (fun () ->
    print_string "thread 2\n";
    loop ()
  ) () in

  print_string "main thread\n";
  loop ()


let test () =
  (* 
     test_print () 
     test_exn ()
     test_threads_cooperatively ();
  test_threads_preemptively ();
  *)
     test_threads_cooperatively ();
  ()
