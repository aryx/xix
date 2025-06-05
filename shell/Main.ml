(*s: Main.ml *)
(* Copyright 2016, 2025 Yoann Padioleau, see copyright.txt *)
open Xix_shell

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
(*e: Main.ml *)
