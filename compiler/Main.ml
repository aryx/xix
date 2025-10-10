(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
(* open Xix_compiler *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
     let argv = CapSys.argv caps in
     Exit.exit caps 
        (Exit.catch (fun () -> 
            CLI.main caps argv))
  )
