open Common
open Types
open Proc_

let syscall_exec cmd args =

  let base = Filename.basename cmd in

  let chan = !Hooks.Chan.chan_of_filename cmd in
  (fun () -> !Hooks.Chan.close chan) |> Common.finalize (fun () ->
    raise Todo
   )

