(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Stdcompat (* for |> *)
open Common

module A = Ast
module E = Env
module R = Rules
module P = Percent

module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* TODO: use proper exn *)
let error loc s =
  failwith (spf "%s:%d: Semantic error, %s" loc.A.file loc.A.line s)

let warning loc s =
  Logs.warn (fun m -> m "warning: %s (at %s:%d)" s loc.A.file loc.A.line)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* A word can become multiple strings!
 * opti? could use a Buffer 
 * invariant: 
 *  - the returned list of strings must not contain any empty string
 *  - less: the returned pattern must contain at least a PPercent
 *)
let rec (eval_word: Ast.loc -> Env.t -> Ast.word -> 
          (Env.values, Percent.pattern) Common.either)= fun loc env (A.W word)->
  let rec aux acc word_elements =
    match word_elements with
    | [] -> 
        if acc = []
        then Left []
        (* less: could look if any PPercent in acc and if not return a Left *)
        else Right (P.P (List.rev acc))
    | x::xs ->
      (match x with
      | A.String s -> aux ((P.PStr s)::acc) xs
      | A.Percent  -> aux (P.PPercent::acc) xs

      | A.Var ((A.SimpleVar v | A.SubstVar (v, _, _)) as vkind)  ->

         let ys = 
           try 
             Hashtbl.find env.E.vars v 
           with Not_found ->
             (* stricter: mk does not complain *)
             if !Flags.strict_mode
             then begin 
               if !Flags.dump_env 
               then Env.dump_env env;
               error loc (spf "variable not found '%s'" v);
             end
             else []
         in
         let ys =
           match vkind with
           | A.SimpleVar _ -> ys
           | A.SubstVar (_, pattern, substs) -> 
               (* recurse! pattern can contain some variable *)
               let pattern = eval_word loc env pattern in
               let subst   = substs |> List.map (eval_word loc env) in
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
                 Logs.debug (fun m -> m "subst = %s" (Common.dump subst));
                 error loc 
                   "pattern or subst does not resolve to a single string"
               )
         in
         (match ys, acc, xs with

         (* variable does not contain anything *)
         | [], [], []  -> 
             Left []
         | [], acc, xs ->
             (* stricter: *)
             warning loc (spf "use of empty variable '%s' in scalar context" v);
             aux acc xs

         (* variable contains a single element (scalar) *)
         | [str], acc, xs -> 
             aux ((P.PStr str)::acc) xs

         (* variable contains many elements (array) *)
         | _::_::_, [], [] -> 
             Left ys
         | _::_::_, acc, xs ->
             (* stricter: *)
             error loc (spf "use of list variable '%s' in scalar context" v)
         )

      | A.Backquoted cmd -> 
        let shellenv = Env.shellenv_of_env env in
        let caps = Cap.exec_and_tmp_caps_UNSAFE () in
        let s = Shell.exec_backquote caps shellenv cmd in
        let ys = Str.split (Str.regexp "[ \t\n]+") s in
        (match acc, xs with
        | [], []  -> Left ys
        (* stricter: *)
        | _ -> error loc (spf "use of `%s` in scalar context" cmd)
        )
      )
  in
  aux [] word


let rec (eval_words: Ast.loc -> Env.t -> Ast.words -> 
         (string list, Percent.pattern list) Common.either)= fun loc env words->
  
  let res = words |> List.map (eval_word loc env) in

  if res |> List.exists (fun x ->
    match x with
    | Left _ -> false
    | Right (P.P xs) -> List.mem P.PPercent xs
  )

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
        | P.PPercent -> raise (Impossible "exists predicate above is wrong")
    ) |> (fun elems -> [elems |> String.concat ""])
  ) |> List.flatten |> (fun xs -> Env.check_values xs; Left xs)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval env targets_ref xs =

  let simples = Hashtbl.create 101 in
  let metas = ref [] in

  let rec instrs xs = 
    xs |> List.iter (fun instr ->
      let loc = instr.A.loc in

      match instr.A.instr with
      | A.Include ws ->
          let res = eval_words loc env ws in
          (match res with
          | Left [file] -> 
              if not (Sys.file_exists file)
              then warning loc (spf "skipping missing include file: %s" file)
              else
                let xs = Parse.parse file in
                (* recurse *)
                instrs xs
          (* new? what does mk does? *)
          | Left [] -> error loc "missing include file"
          (* stricter: force use quotes for filename with spaces or percent *)
          | Right _ | Left (_::_) -> 
              error loc "use quotes for filenames with spaces or %%"
          )

      | A.PipeInclude ws ->
        let res = eval_words loc env ws in
        let recipe = 
          match res with
          | Left xs -> String.concat " " xs
          | Right xs -> raise Todo
        in
        if recipe = ""
        then failwith "missing include program name";
        let shellenv = Env.shellenv_of_env env in
        let caps = Cap.exec_and_tmp_caps_UNSAFE () in
        let tmpfile = Shell.exec_pipecmd caps shellenv recipe in
        let xs = Parse.parse tmpfile in
        (* recurse *)
        instrs xs

      | A.Definition (s, ws) ->
        let xs = 
          match eval_words loc env ws with
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

      | A.Rule r -> 
          let targets = eval_words loc env r.A.targets in
          let prereqs = eval_words loc env r.A.prereqs in
          (match targets, prereqs with
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
              if !targets_ref = [] 
              then targets_ref := targets;

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
          | Left _, Right _ ->
              (* stricter: *)
              error loc "Forgot to use %% for the target"
          )
    )
  in
  instrs xs;

  { R.
    simples = simples;
    metas = !metas
  }, env
