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

let split_when_case cmds =
  cmds |> Common2.span (function
    | (A.Simple (A.Word ("case", false), _)) -> false
    | _ -> true
  )



(*****************************************************************************)
(* Compilation algorithm *)
(*****************************************************************************)

let outcode_seq seq eflag (emit,set,idx) =

  let rec xseq seq eflag =
   (* less set iflast, for if not syntax error checking *)
    seq |> List.iter (fun x -> xcmd x eflag)
  
  and xcmd cmd eflag =
    match cmd with
    | A.EmptyCommand -> ()
    | A.Compound seq -> xseq seq eflag

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

        (* will be executed in a forked child, hence Exit *)
        xcmd cmd1 eflag;
        emit (O.F O.Exit);

        (* will be executed in a children thread, hence Return *)
        set p (O.I !idx);
        xcmd cmd2 eflag;
        emit (O.F O.Return);

        (* will be executed by parent once the children thread finished *)
        set q (O.I !idx);
        emit (O.F O.PipeWait);

    | A.Redir (cmd, (redir_kind, word)) ->
        (* resolve the filename *)
        emit (O.F O.Mark);
        xword word;
        emit (O.F O.Glob);

        (match redir_kind with
        | A.RWrite ->
            emit (O.F O.Write);
            emit (O.I 1);
        (* less: and A.RHere *)
        | A.RRead -> 
            emit (O.F O.Read);
            emit (O.I 0);
        | A.RAppend -> 
            emit (O.F O.Append);
            emit (O.I 1);
        | _ -> failwith ("TODO compile: " ^ Dumper.s_of_cmd cmd)
        );
        
        (* perform the command *)
        xcmd cmd eflag;
        emit (O.F O.Popredir);

    | A.If (cmds, cmd) ->
        xseq cmds false;
        emit (O.F O.If);
        let p = !idx in
        emit (O.I 0);
        xcmd cmd eflag;
        emit (O.F O.Wastrue);
        set p (O.I !idx);

    | A.IfNot cmd ->
        emit (O.F O.IfNot);
        let p = !idx in
        emit (O.I 0);
        xcmd cmd eflag;
        set p (O.I !idx);

    | A.Match (w, ws) ->
        emit (O.F O.Mark);
        xwords ws;
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Match);
        if eflag 
        then emit (O.F O.Eflag);

    | A.Switch (w, cmds) ->

        (match cmds with
        | (A.Simple (A.Word ("case", false), _))::_ -> ()
        | _ -> failwith "case missing in switch"
        );

        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Jump);

        let nextcase = !idx in
        emit (O.I 0);
        let out = !idx in
        emit (O.F O.Jump);
        let leave = !idx in
        emit (O.I 0);
        
        set nextcase (O.I !idx);

        let rec aux cmds =
          match cmds with
          | [] -> ()
          | (A.Simple (A.Word ("case", false), ws))::cmds ->
              emit (O.F O.Mark);
              xwords ws;
              emit (O.F O.Case);
              let nextcase = !idx in
              emit (O.I 0);

              let cmds_for_this_case, other_cases = split_when_case cmds in
              cmds_for_this_case |> List.iter (fun cmd ->
                xcmd cmd eflag
              );
              emit (O.F O.Jump);
              emit (O.I out);
              set nextcase (O.I !idx);
          | _ -> failwith "case missing in switch"
        in
        aux cmds;
        set leave (O.I !idx);
        (* can not call pop_list(), here, otherwise circular deps *)
        emit (O.F O.Popm);

    | A.DelFn w ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.DelFn);

    | A.Not cmd ->
        xcmd cmd eflag;
        emit (O.F O.Not);

    | A.And (cmd1, cmd2) ->
        xcmd cmd1 false;
        emit (O.F O.True);
        let p = !idx in
        xcmd cmd2 eflag;
        set p (O.I !idx)

    | A.Or (cmd1, cmd2) ->
        xcmd cmd1 false;
        emit (O.F O.False);
        let p = !idx in
        xcmd cmd2 eflag;
        set p (O.I !idx)

    | (A.Async _|A.Dup (_, _, _, _)|
       A.While (_, _)|
       A.ForIn (_, _, _)|A.For (_, _)|
       A.Fn (_, _)
       )
       -> failwith ("TODO compile: " ^ Dumper.s_of_cmd cmd)

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

    | A.Dollar w ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Dollar);

    | A.Count w ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Count);
       

    | (A.CommandOutput _|
       A.Index (_, _)|A.Concat (_, _)|A.Stringify _
      )
       -> failwith ("TODO compile: " ^ Dumper.s_of_value w)

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
