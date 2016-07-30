(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

module A = Ast
module E = Env
module R = Rules

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let error loc s =
  failwith (spf "%s:%d: Semantic error, %s" loc.A.file loc.A.line s)

let warning loc s =
  pr2 (spf "warning: %s (at %s:%d)" s loc.A.file loc.A.line)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* A word can become multiple strings!
 * opti? could use a Buffer 
 *)
let rec (eval_word: Ast.loc -> Env.t -> Ast.word -> 
          (Env.values, Rules.pattern) Common.either) = fun loc env (A.W word)->
  let rec aux acc xs =
    match xs with
    | [] -> Right (R.P (List.rev acc))
    | x::xs ->
      (match x with
      | A.String s -> aux ((R.PStr s)::acc) xs
      | A.Percent  -> aux (R.PPercent::acc) xs

      | A.Var ((A.SimpleVar v | A.SubstVar (v, _, _)) as vkind)  ->

         (* stricter: mk does not complain *)
         if not (Hashtbl.mem env.E.vars v)
         then error loc (spf "variable not found '%s'" v);

         let ys = Hashtbl.find env.E.vars v in
         let ys =
           match vkind with
           | A.SimpleVar _ -> ys
           | A.SubstVar (_, pattern, subst) -> 
               (* recurse! pattern can contain some variable *)
               let pattern = eval_word loc env pattern in
               let subst   = eval_word loc env subst in
               (match pattern, subst with
               | Right pattern, Right subst ->
                   ys |> List.map (fun s -> 
                     Percent.match_and_subst pattern subst s
                   )
               (* stricter? what does mk?*)
               | _ -> error loc 
                 "pattern or subst do not resolve to a single string"
               )
         in
         (match ys, acc, xs with
         | [], [], []  -> Right (R.P [])
         | [], acc, xs ->
             (* stricter: *)
             warning loc (spf "use of empty variable '%s' in scalar context" v);
             aux acc xs
         | [str], acc, xs -> aux ((R.PStr str)::acc) xs
         | _::_::_, [], [] -> Left ys
         | _::_::_, acc, xs ->
             (* stricter: *)
             error loc (spf "use of list variable '%s' in scalar context" v)
         )

      | A.Backquoted _ -> error loc "TODO: Backquoted not supported yet in eval"
      )
  in
  aux [] word


let rec (eval_words: Ast.loc -> Env.t -> Ast.words -> 
         (string list, Rules.pattern list) Common.either) = fun loc env words->
  let res = words |> List.map (eval_word loc env) in
  if res |> List.exists (fun x ->
    match x with
    | Left _ -> false
    | Right (R.P xs) -> List.mem R.PPercent xs
  )
  then res |> List.map (function
    | Left xs -> xs |> List.map (fun s -> R.P [R.PStr s])
    | Right x -> [x]
  ) |> List.flatten |> (fun xs -> Right xs)
  else res |> List.map (function
    | Left xs -> xs
    | Right (R.P xs) -> xs |> List.map (function 
        | R.PStr s -> s
        | R.PPercent -> raise (Impossible "exists predicate above is wrong")
    ) |> (fun elems -> [elems |> String.concat ""])
  ) |> List.flatten |> (fun xs -> Left xs)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval env targets xs =

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

      | A.Definition (s, ws) ->
          (* todo: handle override variables *)

          (* stricter? forbid redefinitions *)
          if Hashtbl.mem env.E.vars s
          then error loc (spf "redefinition of %s" s);

          let res = eval_words loc env ws in
          (match res with
          | Left xs -> Hashtbl.replace env.E.vars s xs
          (* stricter: no dynamic patterns *)
          | Right _ -> error loc "use quotes for variable definitions with %"
          )

      | A.Rule r -> error loc "TODO Rule"

    )
  in
  instrs xs;

  { R.
    simples = simples;
    metas = !metas
  }, env
