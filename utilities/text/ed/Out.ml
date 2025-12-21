open Common

(* Displaying text *)

let putchr (e : Env.t) (c : char) =
  (* TODO: if listf *)
  output_char e.out c;
  if c = '\n' then flush e.out

(* pre: str should not contain '\n' ? *)
let putst (e : Env.t) (str : string) : unit =
  (* ugly? should set after putchr \n? also who uses col? *)
  e.col <- 0;
  (* iterate over str and call putchr to get a chance
   * for the listf code above
   *)
  String.iter (putchr e) str;
  putchr e '\n';
  ()

let rec putd (e : Env.t) : unit =
  let r = e.count mod 10 in
  e.count <- e.count / 10;
  if e.count > 0
  then putd e;
  putchr e (Char.chr (Char.code '0' + r))

let printcom (_e : Env.t) : unit =
  failwith "TODO: print_com"
