(* TODO: deprecated, use Exit.exit now *)
let exit (caps : < Cap.exit; .. >) = 
  let _ = caps#exit in
  exit

(* TODO: deprecated, use FS.with_open_in now *)
let open_in (caps : < Cap.open_in; ..>) (file : string) : in_channel = 
  let _ = caps#open_in file in
  (* nosemgrep: do-not-use-open-in *)
  open_in file
