open Common

module A = Ast
module O = Opcode

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST to opcodes.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec split_at_non_assign = function
  | A.Assign (val1, val2, cmd) ->
      let (a,b) = split_at_non_assign cmd in
      (val1, val2)::a, b
  | b -> [], b


(*****************************************************************************)
(* Compilation algorithm *)
(*****************************************************************************)

let outcode_seq seq eflag (emit,set,idx) =

  let rec xseq seq eflag =
   (* less set iflast, for if not checking *)
    seq |> List.iter (fun x -> xcmd x eflag)
  
  and xcmd cmd eflag =
    match cmd with
    | A.Simple (w, ws) -> 
        emit (O.F O.Mark);
        xwords ws;
        xword w;
        emit (O.F O.Simple);
        if eflag 
        then emit (O.F O.Eflag);

    | A.Assign (val1, val2, cmd) ->
        let all_assigns, cmd = 
          split_at_non_assign (A.Assign (val1, val2, cmd)) in
        (match cmd with
        (* A=b; *)
        | A.EmptyCommand -> 
            all_assigns |> List.iter (fun (val1, val2) ->
              emit (O.F O.Mark);
              xword val2;
              emit (O.F O.Mark);
              xword val1;
              emit (O.F O.Assign);
            )

        (* A=b cmd; *)
        | _ -> 
            all_assigns |> List.iter (fun (val1, val2) ->
              emit (O.F O.Mark);
              xword val2;
              emit (O.F O.Mark);
              xword val1;
              emit (O.F O.Local);
            );
            xcmd cmd eflag;
            all_assigns |> List.iter (fun (_, _) ->
              emit (O.F O.Unlocal);
            )
        )
    | A.Pipe (cmd1, cmd2) ->
        emit (O.F O.Pipe);
        emit (O.I 1); (* left fd *)
        emit (O.I 0); (* right fd *)

        let p = !idx in
        emit (O.I 0);
        let q = !idx in
        emit (O.I 0);

        (* will be executed in a forked child *)
        xcmd cmd1 eflag;
        emit (O.F O.Exit);

        (* will be executed in a children thread *)
        set p (O.I !idx);
        xcmd cmd2 eflag;
        emit (O.F O.Return);

        (* will be executed by parent once the children thread finished *)
        set q (O.I !idx);
        emit (O.F O.PipeWait);
        
        
    | _ -> failwith ("TODO compile: " ^ Dumper.s_of_cmd cmd)

 (* Do we need to pass eflag here too?
  * Even though types are mutually recursive because of Backquote, the
  * compilation of backquote does not use eflag!
  *)
  and xword w =
    match w with
    | A.Word (s, _quoted) ->
        emit (O.F O.Word);
        emit (O.S s);

    | A.List ws ->
        xwords ws

    | _ -> failwith ("TODO compile: " ^ Dumper.s_of_value w)

  and xwords ws =
    ws |> List.rev |> List.iter (fun w -> xword w);
    
  in
  xseq seq eflag

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let compile seq =

  (* a growing array *)
  let codebuf = ref [| |] in
  let len_codebuf = ref 0 in
  (* pointer in codebuf *)
  let idx = ref 0 in

  let codebuf_template = Array.create 100 (O.I 0) in

  let emit x =
    (* grow the array if needed *)
    if !idx = !len_codebuf then begin
      len_codebuf := !len_codebuf + 100;
      codebuf := Array.append !codebuf codebuf_template;
    end;

    !codebuf.(!idx) <- x;
    incr idx
  in
  let set idx2 x =
    if idx2 < 0 || idx2 >= !len_codebuf
    then failwith (spf "Bad address %d in set()" idx2);

    !codebuf.(idx2) <- x;
  in
  
  outcode_seq seq !Flags.eflag (emit,set,idx);
  emit (O.F O.Return);
  (* less: O.F O.End *)
  (* less: heredoc, readhere() *)

  (* return the trimmed array *)
  Array.sub !codebuf 0 !idx
  |> (fun x -> if !Flags.dump_opcodes then pr2 (Dumper.s_of_codevec x); x)
