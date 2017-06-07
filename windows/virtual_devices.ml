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


