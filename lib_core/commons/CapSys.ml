
(* nosemgrep: use-caps *)
let chdir (_caps : < Cap.chdir; ..>) = Sys.chdir
(* nosemgrep: do-not-use-argv *)
let argv (_caps : < Cap.argv; .. >) = Sys.argv
(* nosemgrep: use-caps *)
let getenv (_caps : < Cap.env; ..>) = Sys.getenv
(* nosemgrep: use-caps *)
let getenv_opt (_caps : < Cap.env; ..>) = Sys.getenv_opt
(* nosemgrep: use-caps *)
let command (_caps : < Cap.fork; Cap.exec; Cap.wait; .. >) = Sys.command
