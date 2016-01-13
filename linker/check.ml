(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module T = Types

let check h =
  h |> Hashtbl.iter (fun (s, _) v ->
    match v.T.section with
    | T.SXref -> failwith (spf "%s: not defined" s)
    | _ -> ()
  )

