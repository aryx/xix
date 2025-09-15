(* nosemgrep: use-caps *)
let execv (_caps : < Cap.exec; ..>) = Unix.execv
(* nosemgrep: use-caps *)
let execve (_caps : < Cap.exec; ..>) = Unix.execve
(* nosemgrep: use-caps *)
let system (_caps : < Cap.fork; ..>) = Unix.system
(* nosemgrep: use-caps *)
let fork (_caps : < Cap.fork; ..>) = Unix.fork
(* nosemgrep: use-caps *)
let wait (_caps : < Cap.wait; ..>) = Unix.wait
(* nosemgrep: use-caps *)
let waitpid (_caps : < Cap.wait; ..>) = Unix.waitpid
(* nosemgrep: use-caps *)
let kill (_caps : < Cap.kill; ..>) = Unix.kill
(* nosemgrep: use-caps *)
let environment (_caps : < Cap.env; ..>) = Unix.environment
(* nosemgrep: use-caps *)
let chdir (_caps : < Cap.chdir; ..>) = Unix.chdir
