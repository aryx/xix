open Common

module F = File
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

let first_message = ref true

let dispatch fs req request_typ =
  match request_typ with
  | P.T.Version (msize, str) -> 
    (match () with
    | _ when not !first_message ->
      error fs req "version: request not first message"
    | _ when msize < 256 ->
      error fs req "version: message size too small";
    | _ when str <> "9P2000" ->
      error fs req "version: unrecognized 9P version";
    | _ ->
      answer fs {req with P.typ = P.R (P.R.Version (msize, str)) }
    )

  | P.T.Attach (rootfid, _auth_fid, uname, aname) ->
    if uname <> fs.FS.user
    then error fs req (spf "permission defined, %s <> %s" uname fs.FS.user);
    (* less: newlymade, qlock all *)

    (* stricter: *)
    if Hashtbl.mem fs.FS.fids rootfid
    then failwith (spf "fid already used: %d" rootfid);

    (try
       let wid = int_of_string aname in
       let w = Hashtbl.find Globals.windows wid in
       let (_, (qxxx, typ, perm)) = File.root_entry in
       let file_id = qxxx, wid in
       let qid = File.qid_of_fileid file_id typ in
       let file = { 
         F.fid = rootfid; F.qid = qid; F.entry = File.root_entry;
         F.opened = None;
         F.w = w;
       } in
       Hashtbl.add fs.FS.fids rootfid file;
       answer fs {req with P.typ = P.R (P.R.Attach qid) }
     with exn ->
        error fs req (spf "unknown id in attach: %s" aname)
    (* less: incref, qunlock *)
    )
  | _ -> 
    failwith (spf "TODO: req = %s" (P.str_of_msg req))


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
    | P.T x -> dispatch fs req x
    | P.R x ->
      (* less: Ebadfcall *)
      raise (Impossible (spf "got a response request: %s" (P.str_of_msg req)))
    );
    first_message := false
  done
