open Common

module FS = Fileserver
module N = Plan9
module P = Protocol_9P

module Unix1 = Unix
module Unix2 = ThreadUnix

let answer fs res =
  if !Globals.debug
  then pr (P.str_of_msg res);
  P.write_9P_msg res fs.FS.server_fd


let error fs req str =
  let res = { req with P.typ = P.R (P.R.Error str) } in
  answer fs res

(* the master *)
let thread fs =
  (* less: threadsetname *)
  
  while true do
    (* less: care about messagesize? *)
    let req = P.read_9P_msg fs.FS.server_fd in

    (* todo: should exit the whole proc if error *)
    
    if !Globals.debug
    then pr (P.str_of_msg req);

    (match req.P.typ with
    | P.T x -> 
      (match x with
      | P.T.Version (msize, str) -> 
        (* less: should sanity check that it's the first message *)
        (match () with
        | _ when msize < 256 ->
          error fs req "version: message size too small";
        | _ when str <> "9P2000" ->
          error fs req "unrecognized 9P version";
        | _ ->
          answer fs {req with P.typ = P.R (P.R.Version (msize, str)) }
        )
      | _ -> 
        failwith (spf "TODO: req = %s" (P.str_of_msg req))
      )
    | P.R x ->
      raise (Impossible (spf "got a response request: %s" (P.str_of_msg req)))
    )
  done
