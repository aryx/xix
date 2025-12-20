open Common

(* Displaying text *)

let putchr (e : Env.t) (c : char) =
  (* TODO: if listf *)
  output_char e.out c

let putst (e : Env.t) (str : string) : unit =
  (* TODO: iterate over str and call putchr to get a chance
   * for the listf code
   * TODO: e.col <- 0
   *)
  output_string e.out (spf "%s\n" str)
