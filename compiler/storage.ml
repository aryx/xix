
type t =
  | Auto   (* local *)
  | Param  (* parameter *)

  | Extern (* public global defined elsewhere *)
  | Global (* public global defined here *)
  | Static (* Private global *)

type intsize =
  | Int
  | Short
  | Long
  | VLong

type floatsize = 
  | Float
  | Double
