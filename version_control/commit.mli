
type hash = Sha1.t

type t = {
  tree     : Tree.hash;
  parents  : hash list;
  author   : User.t;
  committer: User.t;

  message  : string;
}

(* assumes have already read the 'commit <size>\000' header from input *)
val read: IO.input -> t
