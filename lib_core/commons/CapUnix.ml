let execv (caps : < Cap.exec; ..>) = 
  let _ = caps#exec in
  (* nosemgrep: use-caps *)
  Unix.execv
let execve (caps : < Cap.exec; ..>) = 
  let _ = caps#exec in
  (* nosemgrep: use-caps *)
  Unix.execve
let system (caps : < Cap.fork; ..>) =
  let _ = caps#fork in
  (* nosemgrep: use-caps *)
  Unix.system
let fork (caps : < Cap.fork; ..>) =
  let _ = caps#fork in
  (* nosemgrep: use-caps *)
  Unix.fork
let wait (caps : < Cap.wait; ..>) = 
  let _ = caps#wait in
  (* nosemgrep: use-caps *)
  Unix.wait
let waitpid (caps : < Cap.wait; ..>) = 
  let _ = caps#wait in
  (* nosemgrep: use-caps *)
  Unix.waitpid
let kill (caps : < Cap.kill; ..>) = 
  let _ = caps#kill in
  (* nosemgrep: use-caps *)
  Unix.kill
let environment (caps : < Cap.env; ..>) = 
  let _ = caps#env in
  (* nosemgrep: use-caps *)
  Unix.environment
let chdir (caps : < Cap.chdir; ..>) = 
  let _ = caps#chdir in
  (* nosemgrep: use-caps *)
  Unix.chdir
