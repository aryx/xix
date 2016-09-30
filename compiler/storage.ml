(* Copyright 2016 Yoann Padioleau, see copyright.txt *)

(* No Typedef here, because a typedef is not a storage! *) 
type t =
  | Auto   (* local *)
  | Param  (* parameter *) (* ??? *)

  | Extern (* public global defined elsewhere *)
  | Global (* public global defined here *)
  | Static (* Private global *)
  (* less:  | Inline? | Register *)
 (* with tarzan *)

type intsize =
  | Char
  | Short
  | Int
  | Long
  | VLong
 (* with tarzan *)

type floatsize = 
  | Float
  | Double
 (* with tarzan *)

type stringsize =
  | String
  | Unicode
 (* with tarzan *)
