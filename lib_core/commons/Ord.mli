type t = Less | Equal | Greater

module Operators : sig
  (* Perl inspired *)
  val (<=>): 'a -> 'a -> t
end

val to_comparison : ('a -> 'a -> int) -> 'a -> 'a -> t
