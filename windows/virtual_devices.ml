open Common

module F = File
module W = Window

exception Error of string

let dispatch_open file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in
  match filecode with
  | F.Qroot
    -> ()

let dispatch_close file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in
  match filecode with
  | F.Qroot
    -> ()

let threaded_dispatch_read offset count file =
  let filecode = file.F.entry.F.code in
  let w = file.F.w in

  match filecode with
  | F.Qroot -> raise (Impossible "directories are handled in caller dispatch()")


