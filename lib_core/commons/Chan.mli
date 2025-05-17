
type origin = 
  | File of Fpath.t
  | Stdin
  | String
  | Channel
  | Network

type i = { ic : in_channel; origin : origin }

type o = { oc : out_channel; p : Fpath.t }

(* human readable origin of an input channel (e.g., "foo.c" or "<stdin>") *)
val origin: i -> string
