
type origin = 
  | File of Fpath.t
  | Stdin
  | String
  | Channel
  | Network

type destination = 
  | OutFile of Fpath.t
  | Stdout

type i = { ic : in_channel; origin : origin }

type o = { oc : out_channel; dest : destination }

(* human readable origin of an input channel (e.g., "foo.c" or "<stdin>") *)
val origin: i -> string

(* human readable dest of an output channel (e.g., "foo.c" or "<stdout>") *)
val destination: o -> string
