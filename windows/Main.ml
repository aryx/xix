(*s: Main.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
(* open Xix_windows *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(*s: toplevel [[Main._1]] *)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     Exit.exit caps (Exit.catch (fun () -> CLI.main caps (CapSys.argv caps)))
(*e: toplevel [[Main._1]] *)
  )
(*e: Main.ml *)
