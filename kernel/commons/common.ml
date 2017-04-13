
let (|>) o f = f o

let if_some f = function
  | None -> ()
  | Some x -> f x


exception Todo

exception Impossible of string
