open Common

let thread f =
  (* less: some try ? exit? channel to return result? *)
  f ()
