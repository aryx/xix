(*s: version_control/commit.mli *)

(*s: type Commit.t *)
type t = {
  tree     : Tree.hash; (* the root *)
  (* first commit has no parent, and merge commits have 2 parents *)
  parents  : hash list;
  (* note that User.t contains a time *)
  author   : User.t;
  message  : string;
  (*s: [[Commit.t]] extra fields *)
  committer: User.t;
  (*e: [[Commit.t]] extra fields *)
  (* less: encoding, gpgsig, mergetag, extra *)
}
(*e: type Commit.t *)
(*s: type Commit.hash *)
and hash = Sha1.t
(*e: type Commit.hash *)


(*s: signature Commit.read *)
(* assumes have already read the 'commit <size>\000' hdr from unzipped input *)
val read: IO.input -> t
(*e: signature Commit.read *)
(*s: signature Commit.write *)
(* does not write the header, does not compress *)
val write: t -> 'a IO.output -> unit
(*e: signature Commit.write *)

(*s: signature Commit.show *)
val show: t -> unit
(*e: signature Commit.show *)

(*s: signature Commit.collect_ancestors *)
val collect_ancestors: 
  (hash -> t) ->  hash list -> (hash, bool) Hashtbl.t -> 
  (hash, bool) Hashtbl.t
(*e: signature Commit.collect_ancestors *)

(*s: signature Commit.walk_history *)
val walk_history:  
  (hash -> t) -> (hash -> t -> unit) -> hash ->
  unit
(*e: signature Commit.walk_history *)
(*e: version_control/commit.mli *)
