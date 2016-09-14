
type t =
  | Auto   (* local *)
  | Param  (* parameter *)

  | Extern (* public global defined elsewhere *)
  | Global (* public global defined here *)
  | Static (* Private global *)

type intsize =
  | SInt
  | SShort
  | SLong
  | SVLong

type floatsize = 
  | SFloat
  | SDouble
