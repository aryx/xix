(* Copyright 2016, 2018, 2024, 2025 Yoann Padioleau, see copyright.txt *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     CLI.main caps argv
  )
