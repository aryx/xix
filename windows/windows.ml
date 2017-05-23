open Common
open Window

(* old: was called wpointto in rio-C *)
let window_at_point pt =
  let res = ref None in
  Globals.windows |> Hashtbl.iter (fun _k w ->
    if Rectangle.pt_in_rect pt w.screenr && not w.deleted
    then
      match !res with
      | None -> res := Some w
      | Some x when w.topped > x.topped -> res := Some w
      | _ -> ()
  );
  !res


