
(* nosemgrep: use-caps *)
let chdir (_caps : < Cap.chdir; ..>) = Sys.chdir
(* nosemgrep: do-not-use-argv *)
let argv (_caps : < Cap.argv; .. >) = Sys.argv
(* nosemgrep: use-caps *)
let getenv (_caps : < Cap.env; ..>) = Sys.getenv
