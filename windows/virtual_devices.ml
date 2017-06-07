open Common

module F = File
module W = Window

(* to be catched in thread_fileserver.ml and transformed in an Error 9P msg *)
exception Error of string

let dispatch_open file =
  let filecode = file.F.entry.F.code in
  match filecode with
  | F.Qroot
  | F.Qwinname 
    -> ()

let dispatch_read file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in

  match filecode with
  | F.Qroot -> raise (Impossible "directories are handled in caller dispatch()")
  | F.Qwinname -> 
    let str = w.W.winname in
    if str = ""
    then raise (Error "window has no name")
    else str
    
  
