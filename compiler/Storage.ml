(*s: Storage.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(*s: type [[Storage.t]] *)
(* No Typedef here, because a typedef is not a storage! *) 
type t =
  | Local  (* local *)
  | Param  (* parameter *)

  | Extern (* public global defined elsewhere *)
  | Global (* public global defined here *)
  | Static (* Private global less: could rename Private *)

  (* less:  | Inline? | Register? | ExternRegister? *)
(*e: type [[Storage.t]] *)
[@@deriving show]
(*e: Storage.ml *)
