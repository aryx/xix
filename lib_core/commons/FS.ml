open Fpath_.Operators
open Chan

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Capability-aware filesystem operations.
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* capabilities-aware version of UChan.ml *)
let with_open_in (caps : < Cap.open_in; .. >) f file = 
  let _ = caps#open_in !!file in
  (* nosemgrep: use-caps *)
  UChan.with_open_in f file
let with_open_out (caps : < Cap.open_out; .. >) f file = 
  let _ = caps#open_out !!file in
  (* nosemgrep: use-caps *)
  UChan.with_open_out f file

(* tail recursive efficient version *)
let cat (caps : < Cap.open_in; .. >) (file : Fpath.t) : string list =
  file |> with_open_in caps (fun chan ->
  let rec cat_aux acc ()  =
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan.ic) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () |> List.rev
  )

let remove (caps : < Cap.open_out; ..>) (file : Fpath.t) =
  (* alt: Logs.info (fun m -> m "deleting %s" !!file); *)
  let _ = caps#open_out !!file in
  (* nosemgrep: use-caps *)
  Sys.remove !!file
