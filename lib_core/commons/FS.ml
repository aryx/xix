open Fpath_.Operators

(* capabilities-aware version of UChan.ml *)
let with_open_in (_caps : < Cap.open_in; .. >) = UChan.with_open_in

(* tail recursive efficient version *)
let cat (_caps : < Cap.open_in; .. >) (file : Fpath.t) : string list =
  let chan = open_in !!file in
  let rec cat_aux acc ()  =
      (* cant do input_line chan::aux() cos ocaml eval from right to left ! *)
    let (b, l) = try (true, input_line chan) with End_of_file -> (false, "") in
    if b
    then cat_aux (l::acc) ()
    else acc
  in
  cat_aux [] () |> List.rev |> (fun x -> close_in chan; x)
