open Common

let main () =
  let res = Plan9.bind "/tests/xxx" "/" Plan9.mREPL in
  (*
  let file = Unix.openfile "/test_plan9" [Unix.O_RDWR] 0o666 in
  Unix.close file
  *)
  let dir = Unix.opendir "/" in
  
  let rec aux () =
    try 
      let str = Unix.readdir dir in
      pr str;
      aux ()
    with End_of_file -> ()
  in
  aux ();
  Unix.closedir dir


let _ =
  main ()
