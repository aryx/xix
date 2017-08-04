
type t = {
  url: string;
  fetch: Repository.t (* dst *)  -> Commit.hash (* remote HEAD *);
}
