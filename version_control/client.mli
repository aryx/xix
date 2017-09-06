(*s: version_control/client.mli *)

(*s: type Client.t (version_control/client.mli) *)
type t = {
  url: string;
  fetch: Repository.t (* dst *)  -> Commit.hash (* remote HEAD *);
}
(*e: type Client.t (version_control/client.mli) *)
(*e: version_control/client.mli *)
