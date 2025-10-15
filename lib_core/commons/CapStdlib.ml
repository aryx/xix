(* TODO: deprecated, use Exit.exit now *)
let exit (_caps : < Cap.exit; .. >) = exit

(* TODO: deprecated, use FS.with_open_in now *)
(* nosemgrep: do-not-use-open-in *)
let open_in (_caps : < Cap.open_in; ..>) = open_in
