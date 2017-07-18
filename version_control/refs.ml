
type t =
  | Head (* "HEAD" *)
  | Hash of Commit.hash
  (* pointer (may contain sha1 or another pointer) *)
  | Ref of Common.filename (* e.g. "refs/heads/master" *)
