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
 *  - no support for anonymous field (kencc extension)
 *  - forbid typedefs inside forexpr
 *  - sure? forbid definitions (typedefs, struct, enum) not at toplevel
 *    (confusing anyway?)
 *    (but then would no need blockid for those, or just for nested struct def)
 *  - can not mix qualified and not qualified elements in initializers lists
 * 
 * todo: 
 *  - lineno
 *  - add qualifiers in AST
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error s =
  raise (L.Error (spf "Syntax error: %s" s, !L.line))
let warn s =
  raise (L.Error (spf "Warning: %s" s, !L.line))


(* less: automatic lineno
let mk_e 
*)

(* Defs contain things we lift up in the AST (struct defs, enum defs, typedefs).
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
  (* less: mutable labels: string list; *)

  mutable ids_scope:  (string list) list;
  mutable tags_scope: (string list) list;
  mutable block_scope: Ast.blockid list;

}

(* Check if id already declared? It is complex because at the toplevel
 * it is ok to redeclare the same variable or prototype.
 * So better to do those checks in another phase.
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

(* less: should return an optional list of instructions *)
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
  | _ -> error "pop empty declaration stack"
  )

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

%token <string> TName TTypeName
%token <string * Type.sign * Storage.intsize> TIConst
%token <string * Storage.floatsize> TFConst
%token <string * Storage.stringsize> TString

/*(*-----------------------------------------*)*/
/*(*2 Keywords *)*/
/*(*-----------------------------------------*)*/
%token Tvoid  Tchar Tshort Tint Tlong  Tdouble Tfloat  Tsigned Tunsigned
%token Tstruct Tunion Tenum
%token Ttypedef
%token Tconst Tvolatile  Trestrict Tinline
%token Tauto Tstatic Textern Tregister
%token Tif Telse  Twhile Tdo  Tfor  Tbreak Tcontinue  Treturn Tgoto
%token Tswitch Tcase Tdefault
%token Tsizeof

/*(*-----------------------------------------*)*/
/*(*2 Operators *)*/
/*(*-----------------------------------------*)*/
%token TPlus TMinus  TMul TDiv TMod
%token TEq TEqEq TBang TBangEq
%token TAnd TOr TXor TAndAnd TOrOr
%token TTilde
%token TPlusPlus TMinusMinus
%token TInf TSup  TInfEq TSupEq
%token TInfInf TSupSup
%token <Ast.arithOp> TOpEq

/*(*-----------------------------------------*)*/
/*(*2 Punctuation *)*/
/*(*-----------------------------------------*)*/
%token TOPar TCPar  TOBrace TCBrace TOBra TCBra
%token TComma TSemicolon 
%token TArrow TDot TQuestion TColon

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
       ($2 |> List.map (fun ((id, typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef, init with
          | Left sto, _ -> 
              add_id env id IdIdent;
              VarDecl { v_name = (id, env.block (* 0 *));
                        v_type = typ;
                        v_storage = sto;
                        v_init = init;
                      }
          (* stricter: clang also reports this, but not 5c *)
          | Right (), Some _ ->
              error "initializer with typedef"
          | Right _, None ->
              add_id env id IdTypedef;
              TypeDef { t_name = (id, env.block); t_type = typ; }
         )
        )) 
     }
 | storage_and_type_xdecor block      
     { let (id, ft, sto) = $1 in
       pop_scope env;
       [ FuncDef { f_name = id;
                   f_type = ft;
                   f_body = Block $2;
                   f_storage = sto; 
                 }
       ]
     }

storage_and_type_xdecor: storage_and_type xdecor
    { (* stricter: *)
      if !defs <> []
      then error "move struct or typedef definitions to the toplevel";
      let (id, typ2) = $2 in
      let (sto_or_typedef, typ1) = $1 in
      let typ = typ2 typ1 in
      (match typ, sto_or_typedef with
      | TFunction ft, Left sto -> 
          add_id env id IdIdent;
          new_scope env;
          (* TODO: add params in scope! *)
          (id, ft, sto)
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
       ($2 |> List.map (fun ((id, typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef with
          | Left sto -> 
              add_id env id IdIdent;
              Var { v_name = (id, env.block);
                    v_type = typ;
                    v_storage = sto;
                    v_init = init;
                  }
          (* stricter: *)
          | Right _ -> error "typedefs not at the toplevel are forbidden"
          )
       ))
     }

adlist: xdlist { $1 }

/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

block: tobrace slist tcbrace { $2 }

tobrace: TOBrace { new_scope env }
tcbrace: TCBrace { pop_scope env } 

slist:
 | /*(*empty*)*/ { [] }
 | slist adecl   { $1 @ $2 }
 | slist stmnt   { $1 @ [$2] }

stmnt: 
 | ulstmnt        { $1 }
 | labels ulstmnt { $1 $2 }



ulstmnt: 
 | cexpr TSemicolon { ExprSt $1 }
 /*(* used when do for(...) ; to have an empty statement *)*/
 |       TSemicolon { Block [] }

 | block { Block $1 }

 | Tif TOPar cexpr TCPar stmnt %prec LOW_PRIORITY_RULE 
     { 
       if $5 = Block []
       then warn "empty if body";
       If ($3, $5, Block[]) 
     }
 | Tif TOPar cexpr TCPar stmnt Telse stmnt 
     { 
       if $5 = Block []
       then warn "empty if body";
       if $7 = Block []
       then warn "empty else body";
       If ($3, $5, $7) 
     }
 /*(* stricter: I impose a block, not any stmnt *)*/
 | Tswitch TOPar cexpr TCPar block 
     { (* less: generate (0:int - (0:int - x)) *)
       Switch ($3, $5) 
     }

 | Twhile TOPar cexpr TCPar stmnt                { While ($3, $5) }
 | Tdo stmnt Twhile TOPar cexpr TCPar TSemicolon { DoWhile ($2, $5) }
 | tfor TOPar forexpr TSemicolon zcexpr TSemicolon zcexpr TCPar stmnt
     { pop_scope env;
       For ($3, $5, $7, $9) 
     }

 | Treturn zcexpr TSemicolon { Return ($2) }
 | Tbreak TSemicolon         { Break }
 | Tcontinue TSemicolon      { Continue } 
 | Tgoto tag TSemicolon      { Goto ($2) }

tag: 
 | TName     { $1 }
 | TTypeName { $1 }


tfor: Tfor { new_scope env }

forexpr: 
 | zcexpr { Left $1 }
 | storage_and_type adlist 
     { (* less: introduce a new scope? *)
       Right ($2 |> List.map (fun ((id, typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef with
          | Left sto -> 
                 add_id env id IdIdent;
                 { v_name = (id, env.block);
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
 | TName      TColon  { (fun st -> Label ($1, st)) }
 | Tcase expr TColon  { (fun st -> Case ($2, st)) }
 | Tdefault   TColon  { (fun st -> Default st) }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr:
 | xuexpr { $1 }

 | expr TPlus expr   { Binary ($1, Arith Plus, $3) }
 | expr TMinus expr  { Binary ($1, Arith Minus, $3) }
 | expr TMul expr    { Binary ($1, Arith Mul, $3) }
 | expr TDiv expr    { Binary ($1, Arith Div, $3) }
 | expr TMod expr    { Binary ($1, Arith Mod, $3) }

 | expr TAnd expr    { Binary ($1, Arith And, $3) }
 | expr TXor expr    { Binary ($1, Arith Xor, $3) }
 | expr TOr expr     { Binary ($1, Arith Or, $3) }

 | expr TSupSup expr { Binary ($1, Arith ShiftRight, $3) }
 | expr TInfInf expr { Binary ($1, Arith ShiftLeft , $3) }

 | expr TAndAnd expr { Binary ($1, Logical AndLog, $3) }
 | expr TOrOr expr   { Binary ($1, Logical OrLog, $3) }

 | expr TEqEq expr   { Binary ($1, Logical Eq, $3) }
 | expr TBangEq expr { Binary ($1, Logical NotEq, $3) }

 | expr TInf expr    { Binary ($1, Logical Inf, $3) }
 | expr TSup expr    { Binary ($1, Logical Sup, $3) }
 | expr TInfEq expr  { Binary ($1, Logical InfEq, $3) }
 | expr TSupEq expr  { Binary ($1, Logical SupEq, $3) }


 | expr TEq expr     { Assign (SimpleAssign, $1, $3) }
 | expr TOpEq expr   { Assign (OpAssign $2, $1, $3) }

 | expr TQuestion cexpr TColon expr { CondExpr ($1, $3, $5) }

xuexpr:
 | uexpr { $1 }

 | TOPar qualifier_and_type abdecor TCPar xuexpr  { Cast ($3 $2, $5) }

uexpr:
 | pexpr { $1 }

 | TPlus xuexpr  { Unary (UnPlus, $2) }
 | TMinus xuexpr { Unary (UnMinus, $2) }

 | TBang xuexpr  { Unary (Not, $2) }
 | TTilde xuexpr { Unary (Tilde, $2) }

 | TMul xuexpr  { Unary (DeRef, $2) }
 | TAnd xuexpr  { Unary (GetRef, $2) }

 | TPlusPlus xuexpr   { Prefix (Inc, $2) }
 | TMinusMinus xuexpr { Prefix (Dec, $2) } 

pexpr:
 | TOPar cexpr TCPar { $2 }

 /*(* less: could do implicit declaration of unknown function *)*/
 | pexpr TOPar zelist TCPar { Call ($1, $3) }
 /*(* stricter: was cexpr, but ugly to allow cexpr here *)*/
 | pexpr TOBra expr TCBra  { ArrayAccess ($1, $3) }

 | pexpr TDot tag   { RecordAccess ($1, $3) }
 | pexpr TArrow tag { RecordPtAccess ($1, $3) } 

 | TName   
     { try 
         let (idkind, blockid) = Hashtbl.find env.ids $1 in
         assert (idkind <> IdTypedef);
         Id ($1, blockid)
       with Not_found ->
         (* less: if caller is a Call, then implicit declaration of func? *)
         warn (spf "name not declared: %s" $1);
         Id ($1, 0)
     }

 | TIConst { let (a,b,c) = $1 in Int (a,b,c) } 
 | TFConst { Float (fst $1, snd $1) }
 | string  { String (fst $1, snd $1) }

 | pexpr TPlusPlus   { Postfix ($1, Inc) }
 | pexpr TMinusMinus { Postfix ($1, Dec) } 

 | Tsizeof TOPar qualifier_and_type abdecor TCPar { SizeOf (Right ($4 $3)) }
 | Tsizeof uexpr { SizeOf (Left $2) }

cexpr:
 | expr { $1 }
 | cexpr TComma cexpr { Sequence ($1, $3) }


string:
 | TString        { $1 }
 | string TString 
     { let (s1,t1) = $1 in let (s2,t2) = $2 in
       (* stricter: better error message, 5c just says "syntax error" *)
       if t1 <> t2
       then error "incompatible strings"
       else s1 ^ s2, t1
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
       | [] -> failwith "Impossible: grammar force one element"
       | (Left x)::xs ->
           ArrayInit (x::(xs |> List.map (function
             | Left x -> x
             | Right _ -> error "mixing array and record initializer forbidden"
           )))
       | (Right x)::xs ->
           RecordInit (x::(xs |> List.map (function
             | Right x -> x
             | Left _ -> error "mixing array and record initializer forbidden"
           )))
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
 | TDot tag         { (fun x -> Right ($2, x)) }


/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 Types part 1 (left part of a type) *)*/
/*(*-----------------------------------------*)*/

simple_type:
 | Tchar            { (Type.TChar Type.Signed) }
 | Tsigned Tchar    { (Type.TChar Type.Signed) }
 | Tunsigned Tchar  { (Type.TChar Type.Unsigned) }

 | Tshort           { (Type.TShort Type.Signed) }
 | Tunsigned Tshort { (Type.TShort Type.Unsigned) }

 | Tint             { (Type.TInt Type.Signed) }
 | Tunsigned Tint   { (Type.TInt Type.Unsigned) }
 /*(*bad: should be removed, but for compatibility with plan9 code I keep it*)*/
 | Tunsigned        { (Type.TInt Type.Unsigned) }

 | Tlong            { (Type.TLong Type.Signed) }
 | Tunsigned Tlong  { (Type.TLong Type.Unsigned) }

 | Tlong Tlong      { (Type.TVLong Type.Signed) }
 | Tunsigned Tlong Tlong { (Type.TVLong Type.Unsigned) }


 | Tfloat  { Type.TFloat }
 | Tdouble { Type.TDouble }

 | Tvoid   { Type.TVoid }
/*(* less: allow other combinations in grammar but report error?
   * better than just "syntax error".
   *)*/

su:
 | Tstruct { Ast.Struct }
 | Tunion  { Ast.Union }

tag_opt:
 | tag           { $1 }
 | /*(*empty*)*/ { gensym () }

complex_type:
 | su tag { 
     try 
       let (tagkind, bid) = Hashtbl.find env.tags $2 in
       (* less: assert takind = $1 *)
       let fullname = $2, bid in
       Ast.TStructName ($1, fullname)
     with Not_found ->
       (* todo: should check later that defined somewhere, kinda forward decl *)
       (* todo: add in env.tags? *)
       (* todo: put 0 for block here? *)
       let fullname = $2, env.block in
       Ast.TStructName ($1, fullname)
 }
 | su tag_opt sbody {
     let fullname = $2, env.block in
     (* check if already defined? or conflicting su? do that in typecheck.ml *)
     defs := (StructDef { s_name = fullname; s_kind = $1; s_flds = $3 })::!defs;
     (* todo: add in env.tags!! *)
     Ast.TStructName ($1, fullname)
     (* less: sualign *)
 }


 | Tenum tag   { 
     try 
       let (tagkind, bid) = Hashtbl.find env.tags $2 in
       (* less: assert takind = $1 *)
       let fullname = $2, bid in
       Ast.TEnumName (fullname)
     with Not_found ->
       (* todo: should check later that defined somewhere, kinda forward decl *)
       let fullname = $2, env.block in
       Ast.TEnumName (fullname)
    }
 | Tenum tag_opt TOBrace enum TCBrace {
     let fullname = $2, env.block in
     (* todo: scope?? *)
     (* todo: add in env.tags! *)
     defs := (EnumDef { e_name = fullname; e_constants = $4 })::!defs;
     Ast.TEnumName fullname
 }

 | TTypeName 
     { try 
         let (idkind, bid) = Hashtbl.find env.ids $1 in 
         assert (idkind = IdTypedef);
         Ast.TTypeName ($1, bid) 
       with Not_found -> error (spf "count not find typedef for %s" $1)
     }

type_:
  | simple_type  { Ast.TBase $1 }
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
 *)*/

xdecor:
 | xdecor2                { $1 }
 | TMul qualifiers xdecor { let (id, f) = $3 in id, (fun x -> f (TPointer x)) }

/*(* use tag here too, as can have foo foo; declarations *)*/
xdecor2:
 | tag                
     { $1, (fun x -> x) }
 | TOPar xdecor TCPar 
     { $2 }
 | xdecor2 TOBra zexpr TCBra 
     { let (id, f) = $1 in id, (fun x -> TArray ($3, f x)) }
 (* todo: add scope here. Parameters will be added back in scope
  * before processing the body of a function.
  *)
 | xdecor2 TOPar zparamlist TCPar
     { let (id, f) = $1 in id, (fun x -> TFunction (f x, $3)) }



zparamlist:
 | /*(*empty*)*/ { [], false }
 | paramlist       { $1 }

/*less: name { } */
paramlist:
 | qualifier_and_type xdecor  
     { let (id, typ2) = $2 in
       (* add id in scope? No, because it would add id to toplevel scope.
        * Add id in scope in caller before parsing the body of the function.
        *)
       [{p_name = Some id; p_type = typ2 $1 }], false
     }
 | qualifier_and_type abdecor { [{ p_name = None; p_type = $2 $1 }], false }

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
 | TMul qualifiers          { (fun x -> TPointer x) }
 | TMul qualifiers abdecor1 { (fun x -> $3 (TPointer x)) }
 | abdecor2  { $1 }

abdecor2:
 | abdecor3 { $1 }
 | abdecor2 TOPar zparamlist TCPar { (fun x -> TFunction ($1 x, $3)) }
 | abdecor2 TOBra zexpr TCBra    { (fun x -> TArray ($3, $1 x)) }

abdecor3:
 | TOPar TCPar          { (fun x -> TFunction (x, ([], false)))  }
 | TOBra zexpr TCBra    { (fun x -> TArray ($2, x)) }
 | TOPar abdecor1 TCPar { $2 }


/*(*************************************************************************)*/
/*(*1 Struct/union/enum definition *)*/
/*(*************************************************************************)*/

/*(* todo: scope?? *)*/
sbody: TOBrace edecl TCBrace { $2 }

edecl:
 |       edecl_elem TSemicolon { $1 }
 | edecl edecl_elem TSemicolon { $1 @ $2 }

edecl_elem: qualifier_and_type zedlist
 { $2 |> List.map (fun (id, typ2) -> 
     (* note that this element can introduce a nested struct definition! *)
     let typ1 = $1 in
     let typ = typ2 typ1 in
     { fld_name = Some id; fld_type = typ }
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



/*(* 
   *)*/
enum:
 | TName                
     { add_id env $1 IdEnumConstant; 
       [($1, env.block), None] }
 | TName TEq const_expr 
     { 
      (* note that const_expr can reference enum constants defined before *)
       add_id env $1 IdEnumConstant;
       [($1, env.block), Some $3] }

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


/*(* stricter: impose an order. c then g then t? *)*/
storage_and_type:
 |          qualifiers type_ { Left Storage.Auto, $2 }
 | storage  qualifiers type_ { Left $1, $3 }
 | Ttypedef qualifiers type_ { Right (), $3 }

qualifier_and_type: qualifiers type_ { $2 }

qualifiers:
 | /*(*empty*)*/        { [] }
 | qualifiers qualifier { $1 @ [$2] }
