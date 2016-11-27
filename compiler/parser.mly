%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Many things done at parsing time by 5c are not done here. We do
 * the minimum here. We just return a very simple AST.
 * 
 * Limitations compared to 5c (and sometimes also ANSI C):
 *  - no support for old style parameter declaration
 *    (obsolete practice anyway)
 *  - impose a certain order for the storage, qualifier, and type
 *    (everybody follow this convention anyway)
 *  - no implicit single 'signed' means 'signed int'. Signed has to have
 *    an explicit int-type after.
 *  - no support (yet) for anonymous field (kencc extension)
 *  - forbid typedefs inside forexpr
 *  - sure? forbid definitions (typedefs, struct, enum) not at toplevel
 *    (confusing anyway?)
 *    (but then would no need blockid for those, or just for nested struct def)
 *  - can not mix qualified and not qualified elements in initializers lists
 * 
 * todo: 
 *  - add qualifiers in AST
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  raise (L.Error (spf "Syntax error: %s" s, !L.line))
let warn s =
  raise (L.Error (spf "Warning: %s" s, !L.line))

let mk_e e loc = { e = e; e_loc = loc }
let mk_t t loc = { t = t; t_loc = loc }
let mk_st st loc = { st = st; stmt_loc = loc }


(* Defs contains things we lift up in the AST (struct defs, enums, typedefs).
 * Note that we tag those defs with a blockid, so there is no escape-scope
 * problem.
 *)
let defs =
  ref []

let get_and_reset x =
  let v = !x in
  x := [];
  v

(* to manage scope (used notably to recognize typedefs in the lexer) *)
type env = {
  (* there are mainly two namespaces in C: one for ids and one for tags *)
  ids:  (string, idkind * Ast.blockid) Hashtbl.t;
  tags: (string, tagkind * Ast.blockid) Hashtbl.t;

  mutable block: Ast.blockid;

  mutable ids_scope:  (string list) list;
  mutable tags_scope: (string list) list;
  mutable block_scope: Ast.blockid list;

}

(* Warn if id already declared? No, because at the toplevel
 * it is ok to redeclare the same variable or prototype.
 * So better to do those checks in another phase (in check.ml).
 *)
let add_id env id idkind =
  Hashtbl.add Globals.hids id idkind;
  Hashtbl.add env.ids id (idkind, env.block);
  match env.ids_scope with
  | xs::xss -> env.ids_scope <- (id::xs)::xss
  | [] -> env.ids_scope <- [[id]]

let add_tag env tag tagkind =
  Hashtbl.add env.tags tag (tagkind, env.block);
  match env.tags_scope with
  | xs::xss -> env.tags_scope <- (tag::xs)::xss
  | [] -> env.tags_scope <- [[tag]]



let env = {
  ids = Hashtbl.create 101;
  tags = Hashtbl.create 101;
  block = 0;
  
  ids_scope = [];
  tags_scope = [];
  block_scope = [];
}

(* a few builtins *)
let _ =
  add_id env "USED" IdIdent;
  add_id env "SET" IdIdent;
  ()

let block_counter = ref 0

let gensym_counter = ref 0
let gensym () =
  incr gensym_counter;
  spf "|sym%d|" !gensym_counter


let new_scope env =
  incr block_counter;
  env.block_scope <- env.block :: env.block_scope;
  env.block <- !block_counter;
  env.ids_scope <- []::env.ids_scope;
  env.tags_scope <- []::env.tags_scope;
  ()

let pop_scope env =
  (match env.block_scope, env.ids_scope, env.tags_scope with
  | x::xs, ys::yss, zs::zss -> 
      env.block <- x;
      env.block_scope <- xs;
      ys |> List.iter (fun id ->
        Hashtbl.remove env.ids id;
        Hashtbl.remove Globals.hids id;
      );
      zs |> List.iter (fun tag -> 
        Hashtbl.remove env.tags tag
      );
      env.ids_scope <- yss;
      env.tags_scope <- zss;
  | _ -> raise (Impossible "pop empty declaration stack, grammar wrong")
  )
  (* less: return an optional list of instructions at some point *)

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

%token <Ast.loc * string> TName TTypeName
%token <Ast.loc * string * Type.sign * Storage.intsize> TIConst
%token <Ast.loc * string * Storage.floatsize> TFConst
%token <Ast.loc * string * Storage.stringsize> TString

/*(*-----------------------------------------*)*/
/*(*2 Keywords *)*/
/*(*-----------------------------------------*)*/
%token <Ast.loc> Tvoid  Tchar Tshort Tint Tlong  Tdouble Tfloat  
%token <Ast.loc> Tsigned Tunsigned
%token <Ast.loc> Tstruct Tunion Tenum
%token <Ast.loc> Ttypedef
%token <Ast.loc> Tconst Tvolatile  Trestrict Tinline
%token <Ast.loc> Tauto Tstatic Textern Tregister
%token <Ast.loc> Tif Telse  Twhile Tdo  Tfor  Tbreak Tcontinue  Treturn Tgoto
%token <Ast.loc> Tswitch Tcase Tdefault
%token <Ast.loc> Tsizeof

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/
%token <Ast.loc> TPlus TMinus  TMul TDiv TMod
%token <Ast.loc> TEq TEqEq TBang TBangEq
%token <Ast.loc> TAnd TOr TXor TAndAnd TOrOr
%token <Ast.loc> TTilde
%token <Ast.loc> TPlusPlus TMinusMinus
%token <Ast.loc> TInf TSup  TInfEq TSupEq
%token <Ast.loc> TInfInf TSupSup
%token <Ast.loc * Ast.arithOp> TOpEq

/*(*-----------------------------------------*)*/
/*(*2 Punctuation *)*/
/*(*-----------------------------------------*)*/
%token <Ast.loc> TOPar TCPar  TOBrace TCBrace TOBra TCBra
%token <Ast.loc> TComma TSemicolon 
%token <Ast.loc> TArrow TDot TQuestion TColon

/*(*-----------------------------------------*)*/
/*(*2 Misc *)*/
/*(*-----------------------------------------*)*/
%token TSharp
%token EOF

/*(*************************************************************************)*/
/*(*1 Priorities *)*/
/*(*************************************************************************)*/
/*(* must be at the top so that it has the lowest priority *)*/
%nonassoc LOW_PRIORITY_RULE
/*(* see conflicts.txt *)*/
%nonassoc Telse

/*(* in 5c but not in orig_c.mly *)*/
%left   TSemicolon
%left   TComma
%right  TEq TOpEq
%right  TQuestion TColon

/*(* same than in orig_c.mly *)*/
%left   TOrOr
%left   TAndAnd
%left   TOr
%left   TXor
%left   TAnd
%left   TEqEq TBangEq
%left   TInf TSup TInfEq TSupEq
%left   TInfInf TSupSup
%left   TPlus TMinus
%left   TMul TDiv TMod

/*(* in 5c but not in orig_c.mly *)*/
%right  TMinusMinus TPlusPlus TArrow TDot TOBra TOPar

/*(*************************************************************************)*/
/*(*1 Rules type declaration *)*/
/*(*************************************************************************)*/

%type <Ast.program> prog
%start prog

%%

/*(*************************************************************************)*/
/*(*1 Program *)*/
/*(*************************************************************************)*/
prog: prog1 EOF   { $1 }

prog1:
 |  /*(*empty*)*/ { [] }
 | prog1 xdecl    { $1 @ $2 }

/*(*************************************************************************)*/
/*(*1 Declarations *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 External declarators *)*/
/*(*-----------------------------------------*)*/

xdecl: 
 | storage_and_type        TSemicolon 
     { (* stricter: *)
       if !defs = [] 
       then error "declaration without any identifier";
       get_and_reset defs;
     }
 | storage_and_type xdlist TSemicolon 
     { (* less: could suggest to separate struct/enum def from vardecl *)
       get_and_reset defs @
       ($2 |> List.map (fun (((id, loc), typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef, init with
          | Left sto, _ -> 
              add_id env id IdIdent;
              VarDecl { v_name = (id, env.block (* 0 *));
                        v_loc = loc;
                        v_type = typ;
                        v_storage = sto;
                        v_init = init;
                      }
          (* stricter: clang also reports this, but not 5c *)
          | Right (), Some _ ->
              error "initializer with typedef"
          | Right (), None ->
              add_id env id IdTypedef;
              TypeDef { typedef_name = (id, env.block); 
                        typedef_loc = loc; 
                        typedef_type = typ; 
                      }
          )
        )) 
     }

 | storage_and_type_xdecor block_no_new_scope
     { let ((id, loc), ft, sto) = $1 in
       pop_scope env;
       [ FuncDef { f_name = id;
                   f_loc = loc;
                   f_type = ft;
                   f_body = $2;
                   f_storage = sto; 
                 }
       ]
     }

storage_and_type_xdecor: storage_and_type xdecor
    { (* stricter: *)
      if !defs <> []
      then error "move struct or typedef definitions to the toplevel";
      let ((id, loc), typ2) = $2 in
      let (sto_or_typedef, typ1) = $1 in
      let typ = typ2 typ1 in
      (match typ.t, sto_or_typedef with
      | TFunction ft, Left sto -> 
          (* add in global scope the function name *)
          add_id env id IdIdent;
          (* add in new scope the parameters *)
          new_scope env;
          (* TODO: add params in scope! and return adjusted ft *)
          ((id, loc), ft, sto)
      (* stricter: *)
      | TFunction _, Right _ ->
          error "a function definition can not be a type definition"
      | _, _ -> error "not a function type"
      )
    }

xdlist:
 | xdecor          { [$1, None] }
 | xdecor TEq init { [$1, Some $3] }

 | xdlist TComma xdlist { $1 @ $3 }


/*(*-----------------------------------------*)*/
/*(*2 Automatic declarators *)*/
/*(*-----------------------------------------*)*/

adecl:
 | storage_and_type        TSemicolon 
     { if !defs = [] 
       then error "declaration without any identifier";
       (* stricter: *)
       error "move struct or typedef definitions to the toplevel";
       (* TODO? could allow and just return [] here *)
     }

 | storage_and_type adlist TSemicolon 
     { (* stricter: *)
       if !defs <> [] 
       then error "move struct or typedef definitions to the toplevel";
       ($2 |> List.map (fun (((id, loc), typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef with
          | Left sto -> 
              add_id env id IdIdent;
              mk_st (Var { v_name = (id, env.block);
                           v_loc = loc;
                           v_type = typ;
                           v_storage = sto;
                           v_init = init;
                         }) loc
          (* stricter: *)
          | Right () -> error "typedefs not at the toplevel are forbidden"
          )
       ))
     }

adlist: xdlist { $1 }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

block: tobrace slist tcbrace { mk_st (Block $2) $1 }

tobrace: TOBrace { new_scope env; $1 }
tcbrace: TCBrace { pop_scope env; $1 } 

block_no_new_scope: TOBrace slist TCBrace { mk_st (Block $2) $1 }

slist:
 | /*(*empty*)*/ { [] }
 | slist adecl   { $1 @ $2 }
 | slist stmnt   { $1 @ [$2] }

stmnt: 
 | ulstmnt        { $1 }
 | labels ulstmnt { $1 $2 }



ulstmnt: 
 | cexpr TSemicolon { mk_st (ExprSt $1) $2 }
 /*(* used when do for(...) ; to have an empty statement *)*/
 |       TSemicolon { mk_st (Block []) $1 }

 | block { $1 }

 | Tif TOPar cexpr TCPar stmnt %prec LOW_PRIORITY_RULE 
     { 
       if $5.st = Block []
       then warn "empty if body";
       mk_st (If ($3, $5, mk_st (Block[]) $1)) $1
     }
 | Tif TOPar cexpr TCPar stmnt Telse stmnt 
     { 
       if $5.st = Block []
       then warn "empty if body";
       if $7.st = Block []
       then warn "empty else body";
       mk_st (If ($3, $5, $7)) $1
     }
 /*(* stricter: I impose a block, not any stmnt *)*/
 | Tswitch TOPar cexpr TCPar block 
     { (* less: generate (0:int - (0:int - x)) *)
       mk_st (Switch ($3, $5)) $1
     }

 | Twhile TOPar cexpr TCPar stmnt                { mk_st (While ($3, $5)) $1 }
 | Tdo stmnt Twhile TOPar cexpr TCPar TSemicolon { mk_st (DoWhile ($2, $5)) $1 }
 | tfor TOPar forexpr TSemicolon zcexpr TSemicolon zcexpr TCPar stmnt
     { pop_scope env;
       mk_st (For ($3, $5, $7, $9)) $1
     }

 | Treturn zcexpr TSemicolon { mk_st (Return $2) $1 }
 | Tbreak TSemicolon         { mk_st Break $1 }
 | Tcontinue TSemicolon      { mk_st Continue $1 } 
 | Tgoto tag TSemicolon      { mk_st (Goto (snd $2)) $1 }

tag: 
 | TName     { $1 }
 | TTypeName { $1 }


tfor: Tfor { new_scope env; $1 }

forexpr: 
 | zcexpr 
     { Left $1 }
 | storage_and_type adlist 
     { Right ($2 |> List.map (fun (((id, loc), typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef with
          | Left sto -> 
            add_id env id IdIdent;
            { v_name = (id, env.block);
              v_loc = loc;
              v_type = typ;
              v_storage = sto;
              v_init = init;
            }
          (* stricter: *)
          | Right _ -> error "typedefs inside 'for' are forbidden"
          )
       ))
     }

labels:
 | label        { (fun st -> $1 st)  }
 | labels label { (fun st -> $1 ($2 st)) }

label:
 /*(* less: not tag here? can not conflict with typedef? *)*/
 | TName      TColon  { (fun st -> mk_st (Label (snd $1, st)) $2)  }
 | Tcase expr TColon  { (fun st -> mk_st (Case ($2, st)) $3) }
 | Tdefault   TColon  { (fun st -> mk_st (Default st) $2) }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr:
 | xuexpr { $1 }

 | expr TPlus expr   { mk_e (Binary ($1, Arith Plus, $3)) $2 }
 | expr TMinus expr  { mk_e (Binary ($1, Arith Minus, $3)) $2 }
 | expr TMul expr    { mk_e (Binary ($1, Arith Mul, $3)) $2 }
 | expr TDiv expr    { mk_e (Binary ($1, Arith Div, $3)) $2 }
 | expr TMod expr    { mk_e (Binary ($1, Arith Mod, $3)) $2 }

 | expr TAnd expr    { mk_e (Binary ($1, Arith And, $3)) $2 }
 | expr TXor expr    { mk_e (Binary ($1, Arith Xor, $3)) $2 }
 | expr TOr expr     { mk_e (Binary ($1, Arith Or, $3)) $2 }

 | expr TSupSup expr { mk_e (Binary ($1, Arith ShiftRight, $3)) $2 }
 | expr TInfInf expr { mk_e (Binary ($1, Arith ShiftLeft , $3)) $2 }

 | expr TAndAnd expr { mk_e (Binary ($1, Logical AndLog, $3)) $2 }
 | expr TOrOr expr   { mk_e (Binary ($1, Logical OrLog, $3)) $2 }

 | expr TEqEq expr   { mk_e (Binary ($1, Logical Eq, $3)) $2 }
 | expr TBangEq expr { mk_e (Binary ($1, Logical NotEq, $3)) $2 }

 | expr TInf expr    { mk_e (Binary ($1, Logical Inf, $3)) $2 }
 | expr TSup expr    { mk_e (Binary ($1, Logical Sup, $3)) $2 }
 | expr TInfEq expr  { mk_e (Binary ($1, Logical InfEq, $3)) $2 }
 | expr TSupEq expr  { mk_e (Binary ($1, Logical SupEq, $3)) $2 }


 | expr TEq expr     { mk_e (Assign (SimpleAssign, $1, $3)) $2 }
 | expr TOpEq expr   { mk_e (Assign (OpAssign (snd $2), $1, $3)) (fst $2) }

 | expr TQuestion cexpr TColon expr { mk_e (CondExpr ($1, $3, $5)) $2 }

xuexpr:
 | uexpr { $1 }

 | TOPar qualifier_and_type abdecor TCPar xuexpr  { mk_e (Cast ($3 $2, $5)) $1 }

uexpr:
 | pexpr { $1 }

 | TPlus xuexpr  { mk_e (Unary (UnPlus, $2)) $1 }
 | TMinus xuexpr { mk_e (Unary (UnMinus, $2)) $1 }

 | TBang xuexpr  { mk_e (Unary (Not, $2)) $1 }
 | TTilde xuexpr { mk_e (Unary (Tilde, $2)) $1 }

 | TMul xuexpr  { mk_e (Unary (DeRef, $2)) $1 }
 | TAnd xuexpr  { mk_e (Unary (GetRef, $2)) $1 }

 | TPlusPlus xuexpr   { mk_e (Prefix (Inc, $2)) $1 }
 | TMinusMinus xuexpr { mk_e (Prefix (Dec, $2)) $1 } 

pexpr:
 | TOPar cexpr TCPar { $2 }

 /*(* less: could do implicit declaration of unknown function *)*/
 | pexpr TOPar zelist TCPar { mk_e (Call ($1, $3)) $2 }
 /*(* stricter: was cexpr, but ugly to allow cexpr here *)*/
 | pexpr TOBra expr TCBra  { mk_e (ArrayAccess ($1, $3)) $2 }

 | pexpr TDot tag   { mk_e (RecordAccess ($1, snd $3)) $2 }
 | pexpr TArrow tag { mk_e (RecordPtAccess ($1, snd $3)) $2 } 

 | TName   
     { let (loc, id) = $1 in
       try 
         let (idkind, blockid) = Hashtbl.find env.ids id in
         assert (idkind <> IdTypedef);
         mk_e (Id (id, blockid)) loc
       with Not_found ->
         (* stricter: if caller is Call, still forbid implicit decl of func! *)
         warn (spf "name not declared: %s" id)
         (*Id ($1, 0)*)
     }

 | TIConst { let (loc, a,b,c) = $1 in mk_e (Int (a,b,c))  loc } 
 | TFConst { let (loc, a,b)   = $1 in mk_e (Float (a,b))  loc }
 | string  { let (loc, a,b)   = $1 in mk_e (String (a,b)) loc }

 | pexpr TPlusPlus   { mk_e (Postfix ($1, Inc)) $2 }
 | pexpr TMinusMinus { mk_e (Postfix ($1, Dec)) $2 } 

 | Tsizeof TOPar qualifier_and_type abdecor TCPar 
     { mk_e (SizeOf (Right ($4 $3))) $1 }
 | Tsizeof uexpr 
     { mk_e (SizeOf (Left $2)) $1 }

cexpr:
 | expr { $1 }
 | cexpr TComma cexpr { mk_e (Sequence ($1, $3)) $2 }


string:
 | TString        { $1 }
 | string TString 
     { let (loc1, s1,t1) = $1 in let (_loc2, s2,t2) = $2 in
       (* stricter: better error message, 5c just says "syntax error" *)
       if t1 <> t2
       then error "incompatible strings"
       else loc1, s1 ^ s2, t1
     }


zexpr:
 | /*(*empty*)*/ { None }
 | lexpr         { Some $1 }

/*(*todo: (long expr) is wrapping expr in a (long) cast *)*/
lexpr: expr { $1 }

zcexpr:
 | /*(*empty*)*/ { None }
 | cexpr         { Some $1 }

zelist:
 | /*(*empty*)*/ { [] }
 | elist         { $1 }

elist:
 | expr { [$1] }
 | elist TComma elist { $1 @ $3 }

/*(*************************************************************************)*/
/*(*1 Initializers *)*/
/*(*************************************************************************)*/

init: 
 | expr                  { $1 }
 | TOBrace ilist comma_opt TCBrace 
     { match $2 with
       | [] -> raise (Impossible "grammar force at least one element")
       | (Left x)::xs ->
           mk_e (ArrayInit (x::(xs |> List.map (function
             | Left x -> x
             | Right _ -> error "mixing array and record initializer forbidden"
           )))) $1
       | (Right x)::xs ->
           mk_e (RecordInit (x::(xs |> List.map (function
             | Right x -> x
             | Left _ -> error "mixing array and record initializer forbidden"
           )))) $1
     }

comma_opt:
 | /*(*empty*)*/ { }
 | TComma { }

ilist:
 | init2              { [$1] }
 | ilist TComma init2 { $1 @ [$3] }

init2:
 | init          { Left (None, $1) }
 | qual TEq init { $1 $3 }

qual:
 | TOBra lexpr TCBra { (fun x -> Left (Some $2, x)) }
 | TDot tag          { (fun x -> Right (snd $2, x)) }


/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 Types part 1 (left part of a type) *)*/
/*(*-----------------------------------------*)*/

simple_type:
 | Tchar            { (Type.TChar Type.Signed, $1) }
 /*(* meh, I should remove all Signed variants *)*/
 | Tsigned Tchar    { (Type.TChar Type.Signed, $1) }
 | Tunsigned Tchar  { (Type.TChar Type.Unsigned, $1) }

 | Tshort           { (Type.TShort Type.Signed, $1) }
 | Tunsigned Tshort { (Type.TShort Type.Unsigned, $1) }

 | Tint             { (Type.TInt Type.Signed, $1) }
 | Tunsigned Tint   { (Type.TInt Type.Unsigned, $1) }
 /*(*bad: should be removed, but for compatibility with plan9 code I keep it*)*/
 | Tunsigned        { (Type.TInt Type.Unsigned, $1) }

 | Tlong            { (Type.TLong Type.Signed, $1) }
 | Tunsigned Tlong  { (Type.TLong Type.Unsigned, $1) }

 | Tlong Tlong      { (Type.TVLong Type.Signed, $1) }
 | Tunsigned Tlong Tlong { (Type.TVLong Type.Unsigned, $1) }


 | Tfloat  { (Type.TFloat, $1) }
 | Tdouble { (Type.TDouble, $1) }

 | Tvoid   { (Type.TVoid, $1) }
/*(* less: allow other combinations in grammar but report error?
   * better than just "syntax error"?
   *)*/

su:
 | Tstruct { Ast.Struct, $1 }
 | Tunion  { Ast.Union, $1 }

tag_opt:
 | tag           { snd $1 }
 | /*(*empty*)*/ { gensym () }

complex_type:
 | su tag { 
     let (su, loc) = $1 in
     let (_, id) = $2 in
     try 
       let (_tagkind, bid) = Hashtbl.find env.tags id in
       (* assert takind = $1? let check.ml do this check *)
       let fullname = id, bid in
       mk_t (Ast.TStructName (su, fullname)) loc
     with Not_found ->
       (* will check in check.ml whether struct defined indeed later *)
       let fullname = id, 0 in
       mk_t (Ast.TStructName (su, fullname)) loc
 }
 | su tag_opt sbody {
     let (su, loc) = $1 in
     let id = $2 in
     let fullname = id, env.block in
     (* check if already defined? or conflicting su? do that in check.ml *)
     defs := (StructDef { s_name = fullname; 
                          s_loc = loc;
                          s_kind = su; 
                          s_flds = $3 })::!defs;
     add_tag env id (if su = Struct then TagStruct else TagUnion);
     mk_t (Ast.TStructName (su, fullname)) loc
 }


 | Tenum tag   { 
     let (_, id) = $2 in
     try 
       let (_tagkind, bid) = Hashtbl.find env.tags id in
       let fullname = id, bid in
       mk_t (Ast.TEnumName (fullname)) $1
     with Not_found ->
       let fullname = id, 0 in
       mk_t (Ast.TEnumName (fullname)) $1
    }
 | Tenum tag_opt TOBrace enum TCBrace {
     let id = $2 in
     let fullname = id, env.block in
     defs := (EnumDef { e_name = fullname;
                        e_loc = $1;
                        e_constants = $4 })::!defs;
     add_tag env id TagEnum;
     mk_t (Ast.TEnumName fullname) $1
 }

 | TTypeName 
     { let (loc, id) = $1 in
       try 
         let (idkind, bid) = Hashtbl.find env.ids id in 
         assert (idkind = IdTypedef);
         mk_t (Ast.TTypeName (id, bid)) loc
       with Not_found -> error (spf "count not find typedef for %s" id)
     }

type_:
  | simple_type  { let (t, loc) = $1 in mk_t (Ast.TBase t) loc }
  | complex_type { $1 }

/*(*-----------------------------------------*)*/
/*(*2 Types part 2 (right part of a type) *)*/
/*(*-----------------------------------------*)*/

/*
(* declarator return a couple: 
 *  (name, partial type (a function to be applied to return type))
 *
 * note that with 'int* f(int)' we must return Func(Pointer int,int) and not
 * Pointer (Func(int,int)), so TMul binds before the rest of xdecor.
 * less: handle qualifiers
 *)*/

xdecor:
 | xdecor2                { $1 }
 | TMul qualifiers xdecor 
     { let (id, f) = $3 in 
       id, (fun x -> f (mk_t (TPointer x) $1))
     }

/*(* use tag here too, as can have 'foo foo;' declarations *)*/
xdecor2:
 | tag                
     { (snd $1, fst $1), (fun x -> x) }
 | TOPar xdecor TCPar 
     { $2 }
 | xdecor2 TOBra zexpr TCBra 
     { let (id, f) = $1 in id, (fun x -> mk_t (TArray ($3, f x)) $2) }
 /*(* add parameters in scope in caller, when processing the function body *)*/
 | xdecor2 TOPar zparamlist TCPar
     { let (id, f) = $1 in id, (fun x -> mk_t (TFunction (f x, $3)) $2) }



zparamlist:
 | /*(*empty*)*/ { [], false }
 | paramlist       { $1 }

/*less: name { } */
paramlist:
 | qualifier_and_type xdecor  
     { let ((id, loc), typ2) = $2 in
       (* the final blockid will be assigned when create the scope of the
        * function body.
        *)
       [{p_name = Some (id, -1); 
         p_loc = loc;
         p_type = typ2 $1 }], false
     }
 | qualifier_and_type abdecor 
     { [{ p_name = None;
          p_loc = $1.t_loc;
          p_type = $2 $1 }], false 
     }

 | paramlist TComma paramlist 
     { let (xs, isdot1) = $1 in
       let (ys, isdot2) = $3 in
       (* stricter: 5c does not report *)
       if isdot1
       then error "dots allowed only in last parameter position";
       xs @ ys, isdot2
     }
 | TDot TDot TDot { [], true }


/*(*-----------------------------------------*)*/

abdecor:
 | /*(*empty*)*/ { (fun x -> x) }
 | abdecor1      { $1 }

abdecor1:
 | TMul qualifiers          { (fun x -> mk_t (TPointer x) $1) }
 | TMul qualifiers abdecor1 { (fun x -> $3 (mk_t (TPointer x) $1)) }
 | abdecor2  { $1 }

abdecor2:
 | abdecor3 { $1 }
 | abdecor2 TOPar zparamlist TCPar { (fun x -> mk_t (TFunction ($1 x, $3)) $2) }
 | abdecor2 TOBra zexpr TCBra      { (fun x -> mk_t (TArray ($3, $1 x)) $2) }

abdecor3:
 | TOPar TCPar          { (fun x -> mk_t (TFunction (x, ([], false))) $1)  }
 | TOBra zexpr TCBra    { (fun x -> mk_t (TArray ($2, x)) $1) }
 | TOPar abdecor1 TCPar { $2 }


/*(*************************************************************************)*/
/*(*1 Struct/union/enum definition *)*/
/*(*************************************************************************)*/

/*(* a structure does not define a new scope *)*/
sbody: TOBrace edecl TCBrace { $2 }

edecl:
 |       edecl_elem TSemicolon { $1 }
 | edecl edecl_elem TSemicolon { $1 @ $2 }

edecl_elem: qualifier_and_type zedlist
 { $2 |> List.map (fun ((id, loc), typ2) -> 
     (* note that this element can introduce a nested struct definition! *)
     let typ1 = $1 in
     let typ = typ2 typ1 in
     { fld_name = Some id; 
       fld_loc = loc;
       fld_type = typ }
   )
 }

/*(* so can parse nested struct def without any identifier *)*/
zedlist:
 | /*(*empty*)*/ { [] }
 | edlist        { $1 }

edlist:
 | edecor               { [$1] }
 | edlist TComma edecor { $1 @ [$3] }

edecor: xdecor { $1 }



/*(* TODO which scope ??? global scope?
   *)*/
enum:
 | TName                
     { let (loc, id) = $1 in
       add_id env id IdEnumConstant; 
       [{ecst_name = (id, env.block); ecst_loc = loc; ecst_value = None}] }
 | TName TEq const_expr 
     { let (loc, id) = $1 in
      (* note that const_expr can reference enum constants defined before *)
       add_id env id IdEnumConstant;
       [{ecst_name = (id, env.block); ecst_loc = loc; ecst_value = Some $3}] }

 | enum TComma enum { $1 @ $3 }
 | enum TComma      { $1 }

const_expr: expr { $1 }

/*(*************************************************************************)*/
/*(*1 Storage, qualifiers *)*/
/*(*************************************************************************)*/

storage:
 | Tauto     { Storage.Auto }
 | Tstatic   { Storage.Static }
 | Textern   { Storage.Extern }
 /*(* 5c skips register declarations *)*/
 | Tregister { Storage.Auto }
 | Tinline   { error "inline not supported" }

qualifier:
 | Tconst    { Type.Const }
 | Tvolatile { Type.Volatile }
 | Trestrict { error "restrict not supported" }


/*(* stricter: I impose an order. c(lass) then g(arbage) then t(type). *)*/
storage_and_type:
 |          qualifiers type_ { Left Storage.Auto (* NoStorage?*), $2 }
 | storage  qualifiers type_ { Left $1, $3 }
 | Ttypedef qualifiers type_ { Right (), $3 }

qualifier_and_type: qualifiers type_ { $2 }

qualifiers:
 | /*(*empty*)*/        { [] }
 | qualifiers qualifier { $1 @ [$2] }
