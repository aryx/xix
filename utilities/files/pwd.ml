
(* alt: let _main = Printf.printf "%s\n" (Sys.getcwd()) *)
let _ =
  Cap.main (fun caps -> Console.print caps (Sys.getcwd()))
