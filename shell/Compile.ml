(*s: shell/Compile.ml *)
open Stdcompat (* for |> *)
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

(*s: function [[Compile.split_at_non_assign]] *)
let rec split_at_non_assign = function
  | A.Assign (val1, val2, cmd) ->
      let (a,b) = split_at_non_assign cmd in
      (val1, val2)::a, b
  | b -> [], b
(*e: function [[Compile.split_at_non_assign]] *)
(*s: function [[Compile.split_when_case]] *)
let split_when_case cmds =
  cmds |> Common2.span (function
    | (A.Simple (A.Word ("case", false), _)) -> false
    | _ -> true
  )
(*e: function [[Compile.split_when_case]] *)

(*****************************************************************************)
(* Compilation algorithm *)
(*****************************************************************************)

(*s: function [[Compile.outcode_seq]] *)
let outcode_seq (seq : Ast.cmd_sequence) eflag (emit,set,idx) : unit =

  let rec xseq (seq : Ast.cmd_sequence) eflag : unit =
   (*s: [[Compile.outcode_seq]] in nested [[xseq()]] *)
   (* less set iflast, for if not syntax error checking *)
   seq |> List.iter (fun x -> xcmd x eflag)
   (*e: [[Compile.outcode_seq]] in nested [[xseq()]] *)
  
  and xcmd (cmd : Ast.cmd) eflag : unit =
    match cmd with
    (*s: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.Simple (w, ws) -> 
        emit (O.F O.Mark);
        xwords ws;
        xword w;
        emit (O.F O.Simple);
        (*s: [[Compile.outcode_seq]] in [[A.Simple]] case after emit [[O.Simple]] *)
        if eflag 
        then emit (O.F O.Eflag);
        (*e: [[Compile.outcode_seq]] in [[A.Simple]] case after emit [[O.Simple]] *)
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.EmptyCommand -> ()
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.And (cmd1, cmd2) ->
        xcmd cmd1 false;
        emit (O.F O.True);
        let p = !idx in
        xcmd cmd2 eflag;

        set p (O.I !idx)
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.Or (cmd1, cmd2) ->
        xcmd cmd1 false;
        emit (O.F O.False);
        let p = !idx in
        xcmd cmd2 eflag;

        set p (O.I !idx)
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.Not cmd ->
        xcmd cmd eflag;
        emit (O.F O.Not);
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.Match (w, ws) ->
        emit (O.F O.Mark);
        xwords ws;
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Match);
        (*s: [[Compile.outcode_seq]] in [[A.Match]] case after emit [[O.Match]] *)
        if eflag 
        then emit (O.F O.Eflag);
        (*e: [[Compile.outcode_seq]] in [[A.Match]] case after emit [[O.Match]] *)
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
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
        | _ -> failwith ("TODO compile: " ^ Dumper_.s_of_cmd cmd)
        );
    
        (* perform the command *)
        xcmd cmd eflag;
        emit (O.F O.Popredir);
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
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
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.If (cmds, cmd) ->
        xseq cmds false;
        emit (O.F O.If);
        let p = !idx in
        emit (O.I 0);
        xcmd cmd eflag;
        emit (O.F O.Wastrue);
        set p (O.I !idx);
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.IfNot cmd ->
        emit (O.F O.IfNot);
        let p = !idx in
        emit (O.I 0);
        xcmd cmd eflag;
        set p (O.I !idx);
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
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

        let aux cmds =
          match cmds with
          | [] -> ()
          | (A.Simple (A.Word ("case", false), ws))::cmds ->
              emit (O.F O.Mark);
              xwords ws;
              emit (O.F O.Case);
              let nextcase = !idx in
              emit (O.I 0);

              let cmds_for_this_case, _other_cases = split_when_case cmds in
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
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.Compound seq -> xseq seq eflag
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.Fn (w, cmds) ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Fn);
        let p = !idx in
        (* less: emit str of fn *)
        emit (O.S "Fn String Todo?");
        xseq cmds eflag;
        emit (O.F O.Unlocal);
        emit (O.F O.Return);
        set p (O.I !idx);
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
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
    (*x: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | A.DelFn w ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.DelFn);
    (*e: [[Compile.outcode_seq]] in nested [[xcmd()]] match [[cmd]] cases *)
    | (A.Async _|
       A.Dup (_, _, _, _)|
       A.While (_, _)|
       A.ForIn (_, _, _)|
       A.For (_, _)
       )
       -> failwith ("TODO compile: " ^ Dumper_.s_of_cmd cmd)

 (* Do we need to pass eflag here too?
  * Even though types are mutually recursive because of Backquote, the
  * compilation of backquote does not use eflag!
  *)
  and xword (w : Ast.value) : unit =
    match w with
    (*s: [[Compile.outcode_seq]] in nested [[xword()]] match [[w]] cases *)
    | A.Word (s, _quoted) ->
        emit (O.F O.Word);
        emit (O.S s);
    (*x: [[Compile.outcode_seq]] in nested [[xword()]] match [[w]] cases *)
    | A.List ws ->
        xwords ws
    (*x: [[Compile.outcode_seq]] in nested [[xword()]] match [[w]] cases *)
    | A.Dollar w ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Dollar);
    (*x: [[Compile.outcode_seq]] in nested [[xword()]] match [[w]] cases *)
    | A.Count w ->
        emit (O.F O.Mark);
        xword w;
        emit (O.F O.Count);
    (*e: [[Compile.outcode_seq]] in nested [[xword()]] match [[w]] cases *)
    | (A.CommandOutput _|
       A.Index (_, _)|
       A.Concat (_, _)|
       A.Stringify _
      )
       -> failwith ("TODO compile: " ^ Dumper_.s_of_value w)

  and xwords (ws : Ast.value list) : unit =
    (*s: [[Compile.outcode_seq]] in nested [[xwords()]] *)
    ws |> List.rev |> List.iter (fun w -> xword w);
    (*e: [[Compile.outcode_seq]] in nested [[xwords()]] *)
  in
  xseq seq eflag
(*e: function [[Compile.outcode_seq]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Compile.compile]] *)
let compile (seq : Ast.cmd_sequence) : Opcode.codevec =

  (* a growing array *)
  let codebuf = ref [| |] in
  let len_codebuf = ref 0 in
  (* pointer in codebuf *)
  let idx = ref 0 in

  (*s: [[Compile.compile()]] nested function [[emit]] *)
  let emit x =
    (*s: [[Compile.compile()]] nested function [[emit]] possibly grow [[codebuf]] *)
    (* grow the array if needed *)
    if !idx = !len_codebuf then begin
      len_codebuf := !len_codebuf + 100;
      codebuf := Array.append !codebuf (Array.make 100 (O.I 0));
    end;
    (*e: [[Compile.compile()]] nested function [[emit]] possibly grow [[codebuf]] *)
    !codebuf.(!idx) <- x;
    incr idx
  in
  (*e: [[Compile.compile()]] nested function [[emit]] *)
  (*s: [[Compile.compile()]] nested function [[set]] *)
  let set idx2 x =
    (*s: [[Compile.compile()]] nested function [[set]] array bound checking *)
    if idx2 < 0 || idx2 >= !len_codebuf
    then failwith (spf "Bad address %d in set()" idx2);
    (*e: [[Compile.compile()]] nested function [[set]] array bound checking *)
    !codebuf.(idx2) <- x;
  in
  (*e: [[Compile.compile()]] nested function [[set]] *)

  outcode_seq seq !Flags.eflag (emit, set, idx);
  emit (O.F O.Return);
  (* less: O.F O.End *)
  (* less: heredoc, readhere() *)

  (* return the trimmed array *)
  Array.sub !codebuf 0 !idx
  (*s: [[Compile.compile()]] possibly dump returned opcodes *)
  |> (fun x -> if !Flags.dump_opcodes then Logs.app (fun m -> m "%s" (Dumper_.s_of_codevec x)); x)
  (*e: [[Compile.compile()]] possibly dump returned opcodes *)
(*e: function [[Compile.compile]] *)
(*e: shell/Compile.ml *)
