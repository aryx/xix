
(* No Typedef here, this is not a storage! *) 
type t =
  | Auto   (* local *)
  | Param  (* parameter *) (* ??? *)

  | Extern (* public global defined elsewhere *)
  | Global (* public global defined here *)
  | Static (* Private global *)

  (* less:  | Inline? | Register *)

type intsize =
  | Char
  | Short
  | Int
  | Long
  | VLong

type floatsize = 
  | Float
  | Double

type stringsize =
  | String
  | Unicode
