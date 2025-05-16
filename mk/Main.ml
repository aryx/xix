(* Copyright 2016, 2018, 2024, 2025 Yoann Padioleau, see copyright.txt *)
open Xix_mk

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.catch caps (fun () -> CLI.main caps argv)
  )
