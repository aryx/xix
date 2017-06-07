open Common

module F = File
module FS = Fileserver
module N = Plan9
module P = Protocol_9P
module W = Window

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
  (* Version *)
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

  (* Attach *)
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
       let entry = File.root_entry in
       let file_id = entry.F.code, wid in
       let qid = File.qid_of_fileid file_id entry.F.type_ in
       let file = { 
         F.fid = rootfid; 
         F.qid = qid; 
         F.entry = entry;
         F.opened = None;
         F.w = w;
       } in
       Hashtbl.add fs.FS.fids rootfid file;
       answer fs {req with P.typ = P.R (P.R.Attach qid) }
     with exn ->
        error fs req (spf "unknown id in attach: %s" aname)
    (* less: incref, qunlock *)
    )

  (* Walk *)
  | P.T.Walk (fid, newfid_opt, xs) ->
    (* stricter: *)
    if not (Hashtbl.mem fs.FS.fids fid)
    then failwith (spf "unknown fid %d" fid);

    let file = Hashtbl.find fs.FS.fids fid in
    let wid = file.F.w.W.id in
    (match file, newfid_opt with
    | { F.opened = Some _ }, _ ->
      error fs req "walk of open file"
    | _, Some newfid when Hashtbl.mem fs.FS.fids newfid ->
      (* stricter? failwith or error? *)
      error fs req (spf "clone to busy fid: %d" newfid)
    | _ when List.length xs > P.max_welem ->
      error fs req (spf "name too long: [%s]" (String.concat ";" xs))
    | _ ->
      let file =
        match newfid_opt with
        | None -> file
        | Some newfid ->
          (* clone *)
          (* less: incref on file.w *)
          { file with F.fid = newfid; F.opened = None }
      in
      let rec aux qid entry acc xs =
        match xs with
        | [] -> qid, entry, List.rev acc
        | x::xs ->
          if qid.N.typ <> N.QTDir
          then failwith "not a directory";
          (try 
            (match entry.F.code, x with
            | _Qwsys, ".." -> raise Todo
            | F.Qroot, x ->
              let entry = 
                File.toplevel_entries |> List.find (fun entry -> 
                  entry.F.name = x
                )
              in
              let file_id = entry.F.code, wid in
              let qid = File.qid_of_fileid file_id entry.F.type_ in
              (* continue with other path elements *)
              aux qid entry (qid::acc) xs
            (* todo: Wsys, snarf *)
            | _ -> 
              raise (Impossible "Not a directory")
            )
          with Not_found ->
            raise Not_found
          )
      in
      (try 
        let final_qid, final_entry, qids = 
          aux file.F.qid file.F.entry [] xs
        in
        file.F.qid <- final_qid;
        file.F.entry <- final_entry;
        answer fs { req with P.typ = P.R (P.R.Walk qids) }
      with exn ->
        newfid_opt |> Common.if_some (fun newfid ->
          Hashtbl.remove fs.FS.fids newfid
        );
        (match exn with
        | Not_found -> 
          error fs req "file does not exist"
        | Failure "not a directory" ->
          error fs req "not a directory"
        | _ -> raise exn
        )
      )
    )

  (* Open *)
  (* Read *)
  (* Clunk *)

  (* Other *)
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
