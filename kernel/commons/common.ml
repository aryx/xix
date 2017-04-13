
let (|>) o f = f o

let if_some f = function
  | None -> ()
  | Some x -> f x


exception Todo

exception Impossible of string

let is_power_of_2 x =
  x <> 0 && 
  x land (x - 1) = 0 

let round_up x pow2 =
  assert (is_power_of_2 pow2);
  (x + (pow2 - 1)) land (lnot (pow2 - 1))

(*TODO! let _ = assert(round_up 2045 1024 = 3072) *)
