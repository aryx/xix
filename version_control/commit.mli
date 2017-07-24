
type t = {
  tree     : Tree.hash;
  parents  : hash list;
  author   : User.t;
  committer: User.t;

  message  : string;
}
and hash = Sha1.t


(* assumes have already read the 'commit <size>\000' hdr from unzipped input *)
val read: IO.input -> t
(* does not write the header, does not compress *)
val write: t -> 'a IO.output -> unit

val show: t -> unit
