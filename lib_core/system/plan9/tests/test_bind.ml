open Common

let main (caps : < Cap.bind; Cap.stdout; ..>) =
  let res = 
    Plan9.bind caps (Fpath.v "/tests/bytecode") (Fpath.v "/tests/test") Plan9.MRepl
  in
  (*
  let file = Unix.openfile "/test_plan9" [Unix.O_RDWR] 0o666 in
  Unix.close file
  *)
  let dir = Unix.opendir "/tests/test" in
  
  let rec aux () =
    try 
      let str = Unix.readdir dir in
      Console.print caps str;
      aux ()
    with End_of_file -> ()
  in
  aux ();
  Unix.closedir dir


let _ =
  Cap.main (fun caps -> main caps)
