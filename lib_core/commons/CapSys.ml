
let chdir (caps : < Cap.chdir; ..>) = 
  let _ = caps#chdir in
  (* nosemgrep: use-caps *)
  Sys.chdir

let argv (caps : < Cap.argv; .. >) = 
  let _ = caps#argv in
  (* nosemgrep: do-not-use-argv *)
  Sys.argv

let getenv (caps : < Cap.env; ..>) = 
  let _ = caps#env in
  (* nosemgrep: use-caps *)
  Sys.getenv
(* TODO: add to ocaml-light
let getenv_opt (caps : < Cap.env; ..>) = 
  let _ = caps#env in
  (* nosemgrep: use-caps *)
  Sys.getenv_opt
*)

let command (caps : < Cap.fork; Cap.exec; Cap.wait; .. >) cmd = 
  let _ = caps#fork in
  let _ = caps#exec cmd in
  let _ = caps#wait in
  (* nosemgrep: use-caps *)
  Sys.command cmd
