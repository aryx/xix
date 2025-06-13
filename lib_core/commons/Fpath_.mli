
val of_strings : string list -> Fpath.t list
val to_strings : Fpath.t list -> string list

module Operators : sig
  (* Fpath.add_seg = Fpath.(/) *)
  val ( / ) : Fpath.t -> string -> Fpath.t

  (* Fpath.append = Fpath.(//) *)
  val ( // ) : Fpath.t -> Fpath.t -> Fpath.t

  (* Fpath.to_string *)
  val ( !! ) : Fpath.t -> string
end
