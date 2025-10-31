(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
(* open Xix_windows *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     Exit.exit caps (Exit.catch (fun () -> CLI.main caps (CapSys.argv caps)))
  )
