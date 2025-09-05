
(* TODO: Lt, Eq, Gt, but need to also modidfy semgrep-libs *)
type t = Less | Equal | Greater

let to_comparison f = 
  fun x y ->
    let res = f x y in
    if res < 0 then Less else if res > 0 then Greater else Equal

module Operators = struct
let (<=>) a b = 
  if a = b 
  then Equal
  else 
    if a < b
    then Less
    else Greater

end
