(*s: mk/Main.ml *)
(* Copyright 2016, 2018, 2024, 2025 Yoann Padioleau, see copyright.txt *)
open Xix_mk

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: toplevel [[Main._1]] *)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.exit caps 
        (Exit.catch (fun () -> 
            CLI.main caps argv))
  )
(*e: toplevel [[Main._1]] *)
(*e: mk/Main.ml *)
