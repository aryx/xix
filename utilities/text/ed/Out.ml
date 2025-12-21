open Common

(* Displaying text *)

let putchr (e : Env.t) (c : char) =
  (* TODO: if listf *)
  output_char e.out c;
  if c = '\n' then flush e.out

let putst (e : Env.t) (str : string) : unit =
  (* iterate over str and call putchr to get a chance
   * for the listf code above
   *)
  e.col <- 0;
  String.iter (putchr e) str;
  putchr e '\n';
  ()

let printcom (_e : Env.t) : unit =
  failwith "TODO: print_com"
