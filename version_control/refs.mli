
type t =
  | Head (* "HEAD" *)
  | Ref of Common.filename (* e.g. "refs/heads/master" *)

type ref_content =
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer again) *)
  | OtherRef of Common.filename
