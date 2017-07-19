
type refname = string (* e.g. "refs/heads/master" *)

type t =
  | Head
  | Ref of refname

type ref_content =
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer again) *)
  | OtherRef of refname

val default_head_content: ref_content

val is_valid_refname: refname -> bool

val read: IO.input -> ref_content
val write: ref_content -> unit IO.output -> unit
