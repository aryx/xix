(*s: Out.ml *)
(* Copyright 2025 Yoann Padioleau, see copyright.txt *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Displaying text *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(*s: function [[Out.putchr]] *)
let putchr (e : Env.t) (c : char) =
  (* TODO: if listf *)
  output_char e.out c;
  if c = '\n' then flush e.out
(*e: function [[Out.putchr]] *)

(*s: function [[Out.putst]] *)
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
(*e: function [[Out.putst]] *)

(*s: function [[Out.putshst]] *)
(* origin: put shell string? *)
let putshst (e : Env.t) (str : string) : unit =
  (* no diff between rune and chars in oed *)
  putst e str
(*e: function [[Out.putshst]] *)

(*s: function [[Out.putd]] *)
(* display e.count *)
let rec putd (e : Env.t) : unit =
  let r = e.count mod 10 in
  e.count <- e.count / 10;
  if e.count > 0
  then putd e;
  putchr e (Char.chr (Char.code '0' + r))
(*e: function [[Out.putd]] *)
(*e: Out.ml *)
