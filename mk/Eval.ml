(*s: mk/Eval.ml *)
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common
open Either
open Fpath_.Operators

module A = Ast
module E = Env
module R = Rules
module P = Percent

module Set = Set_

open Rules (* for the fields *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(*s: function [[Eval.error]] *)
let error (loc : Ast.loc) (s : string) =
  failwith (spf "%s:%d: Semantic error, %s" !!(loc.A.file) loc.A.line s)
(*e: function [[Eval.error]] *)

(*s: function [[Eval.warning]] *)
let warning (loc : Ast.loc) (s : string) : unit =
  Logs.warn (fun m -> m "warning: %s (at %s:%d)" s !!(loc.A.file) loc.A.line)
(*e: function [[Eval.warning]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Eval.eval_word]] *)
(* A word can become multiple strings!
 * opti? could use a Buffer 
 * invariant: 
 *  - the returned list of strings must not contain any empty string
 *  - less: the returned pattern must contain at least a PPercent
 *)
let rec eval_word (caps: < Cap.fork; Cap.exec; .. >) (loc: Ast.loc) (env : Env.t)  (wd : Ast.word) :
          (string list, Percent.pattern) Either.t =
  let (Ast.W word) = wd in
  let rec aux acc word_elements =
    match word_elements with
    | [] -> 
        if acc = []
        then Left []
        (* less: could look if any PPercent in acc and if not return a Left
         * TODO: should do that! bug otherwise? write test case?
         *)
        else Right (P.P (List.rev acc))
    | x::xs ->
      (match x with
      | A.String s -> aux ((P.PStr s)::acc) xs
      | A.Percent  -> aux (P.PPercent::acc) xs

      (* ocaml-light: | A.Var ((A.SimpleVar v | A.SubstVar (v, _, _)) as vkind) *)
      | A.Var ((A.SimpleVar _(*v*) | A.SubstVar (_(*v*), _, _)) as vkind)  ->
         let v =
           match vkind with
           | A.SimpleVar v -> v
           | A.SubstVar (v, _, _) -> v
         in
         let ys = 
           try 
             Hashtbl.find env.E.vars v 
           with Not_found ->
             (*s: [[Eval.eval_word()]] when [[Not_found]] exn thrown for var [[v]] *)
             (* stricter: mk does not complain *)
             if !Flags.strict_mode
             then begin 
               if !Flags.dump_env 
               then Env.dump_env env;
               error loc (spf "variable not found '%s'" v);
             end;
             (*e: [[Eval.eval_word()]] when [[Not_found]] exn thrown for var [[v]] *)
             []
         in
         (*s: [[Eval.eval_word()]] adjust [[ys]] for [[SubstVar]] case *)
         let ys =
           match vkind with
           | A.SimpleVar _ -> ys
           | A.SubstVar (_, pattern, substs) -> 
             (* recurse! pattern can contain some variable *)
             let pattern = eval_word caps loc env pattern in
             let subst   = substs |> List.map (eval_word caps loc env) in
             (match pattern, subst with
             | Right pattern, [Right subst] ->
                 ys |> List.map (fun s -> 
                   Percent.match_and_subst pattern subst s
                 )
             (* todo: ugly, should be more general, works only for 
              * subst like ${OPAM_LIBS:%=-I $OPAM/%}
             *)
             | Right pattern, [Right (P.P [P.PStr x]); Right subst] ->
                 ys |> List.map (fun s -> 
                   [x;Percent.match_and_subst pattern subst s]
                 ) |> List.flatten
             (* stricter? what does mk?*)
             | _ -> 
               Logs.debug (fun m -> m "subst = %s" (Dumper.dump subst));
               error loc 
                 "pattern or subst does not resolve to a single string"
             )
         in
         (*e: [[Eval.eval_word()]] adjust [[ys]] for [[SubstVar]] case *)
         (match ys, acc, xs with
         (*s: [[Eval.eval_word]] when [[Var v]] case, matching [[ys, acc, xs]] *)
         (* variable contains a single element (scalar) *)
         | [str], acc, xs -> 
             aux ((P.PStr str)::acc) xs
         (*x: [[Eval.eval_word]] when [[Var v]] case, matching [[ys, acc, xs]] *)
         (* variable contains many elements (array) *)
         | _::_::_, [], [] -> 
             Left ys
         (*x: [[Eval.eval_word]] when [[Var v]] case, matching [[ys, acc, xs]] *)
         (* variable does not contain anything *)
         | [], [], []  -> 
             Left []
         (*x: [[Eval.eval_word]] when [[Var v]] case, matching [[ys, acc, xs]] *)
         | [], acc, xs ->
             (* stricter: *)
             warning loc (spf "use of empty variable '%s' in scalar context" v);
             aux acc xs
         (*x: [[Eval.eval_word]] when [[Var v]] case, matching [[ys, acc, xs]] *)
         | _::_::_, _acc, _xs ->
             (* stricter: *)
             error loc (spf "use of list variable '%s' in scalar context" v)
         (*e: [[Eval.eval_word]] when [[Var v]] case, matching [[ys, acc, xs]] *)
         )
      (*s: [[Eval.eval_word()]] match [[x]] other cases *)
      | A.Backquoted cmd -> 
        let shellenv = Env.shellenv_of_env env in
        let s = Shell.exec_backquote caps shellenv cmd in
        let ys = Str.split (Str.regexp "[ \t\n]+") s in
        (match acc, xs with
        | [], []  -> Left ys
        (* stricter: *)
        | _ -> error loc (spf "use of `%s` in scalar context" cmd)
        )
      (*e: [[Eval.eval_word()]] match [[x]] other cases *)
      )
  in
  aux [] word
(*e: function [[Eval.eval_word]] *)

(*s: function [[Eval.eval_words]] *)
let eval_words (caps :  < Cap.fork; Cap.exec; .. >) (loc : Ast.loc) (env : Env.t) (words : Ast.words) :
         (string list, Percent.pattern list) Either.t =
  
  let res = words |> List.map (eval_word caps loc env) in
  (*s: [[Eval.eval_words()]] nested function [[contain_percent]] *)
  let contain_percent xs = 
    xs |> List.exists (function
    | Left _ -> false
    | Right (P.P xs) -> List.mem P.PPercent xs
    )
  in
  (*e: [[Eval.eval_words()]] nested function [[contain_percent]] *)
  if contain_percent res 
  (* a list of patterns *)
  then res |> List.map (function
    | Left xs -> xs |> List.map (fun s -> P.P [P.PStr s])
    | Right x -> [x]
  ) |> List.flatten |> (fun xs -> List.iter Percent.check_pattern xs; Right xs)
  (* a list of strings *)
  else res |> List.map (function
    | Left xs -> xs
    | Right (P.P xs) -> xs |> List.map (function 
        | P.PStr s -> s
        | P.PPercent -> raise (Impossible "contain_percent above is buggy then")
    ) |> (fun elems -> [elems |> String.concat ""])
  ) |> List.flatten |> (fun xs -> Env.check_values xs; Left xs)
(*e: function [[Eval.eval_words]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Eval.eval]] *)
let eval (caps : < Cap.fork; Cap.exec; .. >) env targets_ref (xs : Ast.instr list) : Rules.rules * Env.t =
  let simples = Hashtbl.create 101 in
  let metas = ref [] in

  let rec instrs xs = 
    xs |> List.iter (fun instr ->
      let loc = instr.A.loc in
      match instr.A.instr with
      (*s: [[Eval.eval()]] match instruction kind cases *)
      | A.Definition (s, ws) ->
        let xs = 
          match eval_words caps loc env ws with
          | Left xs -> xs
          (* stricter: no dynamic patterns *)
          | Right _ -> error loc "use quotes for variable definitions with %"
        in
        (try 
          Env.add_var env s xs
         with Env.Redefinition s ->
           error loc (spf "redefinition of %s" s)
        );
        Hashtbl.add env.E.vars_we_set s true;
      (*x: [[Eval.eval()]] match instruction kind cases *)
      | A.Include ws ->
          let res = eval_words caps loc env ws in
          (match res with
          | Left [file] -> 
              if not (Sys.file_exists file)
              then warning loc (spf "skipping missing include file: %s" file)
              else
                let xs = Parse.parse (Fpath.v file) in
                (* recurse *)
                instrs xs
          (* new? what does mk does? *)
          | Left [] -> error loc "missing include file"
          (* stricter: force use quotes for filename with spaces or percent *)
          | Right _ | Left (_::_) -> 
              error loc "use quotes for filenames with spaces or %%"
          )
      (*x: [[Eval.eval()]] match instruction kind cases *)
      | A.Rule r -> 
          let targets = eval_words caps loc env r.A.targets in
          let prereqs = eval_words caps loc env r.A.prereqs in
          (match targets, prereqs with
          (*s: [[Eval.eval()]] when [[Rule r]] case, match [[targets]], [[prereqs]] cases *)
          (* regular rules *)
          | Left targets, Left prereqs ->
              let rfinal = { R.
                             targets = targets; 
                             prereqs = prereqs;
                             attrs = Set.of_list r.A.attrs;
                             recipe = r.A.recipe;
                             loc = loc;
                           } in

              targets |> List.iter (fun target ->
                (* less: could check if already there, in which case
                 * need to Hashtbl.replace, not add.
                 * todo: could have a :O: attribute clearly identifying
                 * that you overwrite a previous rule!
                 *)
                Hashtbl.add simples target rfinal
              );
              (*s: [[Eval.eval()]] adjusts [[targets_ref]] when first simple targets in mkfile *)
              if !targets_ref = [] 
              then targets_ref := targets;
              (*e: [[Eval.eval()]] adjusts [[targets_ref]] when first simple targets in mkfile *)
          (*x: [[Eval.eval()]] when [[Rule r]] case, match [[targets]], [[prereqs]] cases *)
          (* meta rules *)
          | Right targets, Right prereqs ->
              let rfinal = { R.
                             targets = targets; 
                             prereqs = prereqs;
                             attrs = Set.of_list r.A.attrs;
                             recipe = r.A.recipe;
                             loc = loc;
                           } in
              metas |> Common.push rfinal
          (*x: [[Eval.eval()]] when [[Rule r]] case, match [[targets]], [[prereqs]] cases *)
          (* it is ok to have a % only for the target to allow
           * for instance rules such as %.o: $HFILES
           *)
          | Right targets, Left prereqs ->
              let rfinal = { R.
                             targets = targets; 
                             prereqs = prereqs 
                               |> List.map (fun s -> P.P [P.PStr s]);
                             attrs = Set.of_list r.A.attrs;
                             recipe = r.A.recipe;
                             loc = loc;
                           } in
              metas |> Common.push rfinal
          (*x: [[Eval.eval()]] when [[Rule r]] case, match [[targets]], [[prereqs]] cases *)
          | Left _, Right _ ->
              (* stricter: *)
              error loc "Forgot to use %% for the target"
          (*e: [[Eval.eval()]] when [[Rule r]] case, match [[targets]], [[prereqs]] cases *)
          )
      (*x: [[Eval.eval()]] match instruction kind cases *)
      | A.PipeInclude ws ->
        let res = eval_words caps loc env ws in
        let recipe = 
          match res with
          | Left xs -> String.concat " " xs
          | Right _xs -> raise Todo
        in
        if recipe = ""
        then failwith "missing include program name";
        let shellenv = Env.shellenv_of_env env in
        let tmpfile = Shell.exec_pipecmd caps shellenv recipe in
        let xs = Parse.parse (Fpath.v tmpfile) in
        (* recurse *)
        instrs xs
      (*e: [[Eval.eval()]] match instruction kind cases *)
    )
  in
  instrs xs;
  { R. simples = simples; metas = !metas}, env
(*e: function [[Eval.eval]] *)
(*e: mk/Eval.ml *)
