
let main () =
  (*
    flush stdout; 
    print_string "Hello World\n";
  *)
  try 
    let _n = Unix.write Unix.stdout "hello world\n" 0 12 in
    while true do
      ()
    done
  with
   Unix.Unix_error (code, cmd, args) ->
     failwith (Printf.sprintf "Unix error: %s with %s(%s)" 
                 (Unix.error_message code) cmd args)

let _ =
  main ()
