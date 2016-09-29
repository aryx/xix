%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast
module L = Location_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to 5c (and sometimes also ANSI C):
 *  - no support for old style parameter declaration
 *    (obsolete practice anyway)
 *  - impose a certain order for the storage, qualifier, and types
 *    (everybody follow this convention anyway)
 *  - no implicit single 'signed' means 'signed int'. Signed has to have
 *    an int-type after.
 *  - no support for anonymous field (kencc extension)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: automatic lineno
let mk_e 
*)

(* Defs contain things we lift up in the AST (struct defs, enum defs, typedefs).
 * Note that by tagging those defs with a blockid, there is no escape-scope
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
  ids: (string, idkind * Ast.blockid) Hashtbl.t;
  tags: (string, tagkind * Ast.blockid) Hashtbl.t;
  mutable block: Ast.blockid;

  mutable ids_scope: ((string * (idkind * Ast.blockid)) list) list;
  mutable tags_scope: ((string * (tagkind * Ast.blockid)) list) list;
  mutable block_scope: Ast.blockid list;

}

let env = {
  ids = Hashtbl.create 101;
  tags = Hashtbl.create 101;
  block = 0;
  
  ids_scope = [];
  tags_scope = [];
  block_scope = [];
}

let block_counter = ref 0
let gensym_counter = ref 0
let gensym () =
  incr gensym_counter;
  spf "|sym%d|" !gensym_counter

let error s =
  raise (L.Error (spf "Syntax error: %s" s, !L.line))

%}

/*(*************************************************************************)*/
/*(*1 Tokens *)*/
/*(*************************************************************************)*/

%token <string> TName TTypeName
%token <string * Type.sign * Storage.intsize> TConst
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
     { get_and_reset defs @
       ($2 |> List.map (fun ((id, typ2), init) ->
          let (sto_or_typedef, typ1) = $1 in
          let typ = typ2 typ1 in
          (match sto_or_typedef, init with
          | Left sto, _ -> 
              VarDecl { v_name = (id, env.block);
                        v_type = typ;
                        v_storage = sto;
                        v_init = init;
                      }
          (* stricter: clang also reports this, but not 5c *)
          | Right (), Some _ ->
              error "initializer with typedef"
          | Right _, None ->
              (* todo: proper scope handling, add in env.ids *)
              Hashtbl.add Globals.hids id Ast.IdTypedef;
              Hashtbl.add env.ids id (Ast.IdTypedef, env.block);

              TypeDef { t_name = (id, env.block);
                        t_type = typ;
                      }
         )
        )) 
     }
 | storage_and_type xdecor block      
     { get_and_reset defs @
       [ let (id, typ2) = $2 in
         let (sto_or_typedef, typ1) = $1 in
         let typ = typ2 typ1 in
         (match typ, sto_or_typedef with
         | TFunction ft, Left sto -> 
             FuncDef { f_name = id;
                       f_type = ft;
                       f_body = $3;
                       f_storage = sto; }
         (* stricter: *)
         | TFunction _, Right _ ->
             error "a function definition can not be a type definition"
         | _, _ -> error "not a function type"
         )
       ]
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
     { [StmtTodo] }
 | storage_and_type adlist TSemicolon 
     { [StmtTodo] }

adlist: xdlist { $1 }


/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

block: TOBrace slist TCBrace { $2 }

slist:
 | /*(*empty*)*/ { [] }
 | slist adecl { $1 @ $2 }
 | slist stmnt { $1 @ [$2] }

stmnt: 
 | ulstmnt        { StmtTodo }
 | labels ulstmnt { StmtTodo }

 | error TSemicolon { error "error before semicolon" }

ulstmnt:
 | zcexpr TSemicolon { }
 | block { }
 | Tif TOPar cexpr TCPar stmnt %prec LOW_PRIORITY_RULE { }
 | Tif TOPar cexpr TCPar stmnt Telse stmnt { }
 | Twhile TOPar cexpr TCPar stmnt { }
 | Tdo stmnt Twhile TOPar cexpr TCPar TSemicolon { }
 | Tfor TOPar forexpr TSemicolon zcexpr TSemicolon zcexpr TCPar stmnt { }
 | Treturn zcexpr TSemicolon { }
 | Tbreak TSemicolon { }
 | Tcontinue TSemicolon { }
 | Tswitch TOPar cexpr TCPar stmnt { }
 | Tgoto tag TSemicolon { }

tag: 
 | TName     { $1 }
 | TTypeName { $1 }



forexpr: 
 | zcexpr { }
 | storage_and_type adlist { }

labels:
 | label { }
 | labels label { }

label:
 | TName TColon { }
 | Tcase expr TColon { }
 | Tdefault TColon { }

/*(*************************************************************************)*/
/*(*1 Expressions *)*/
/*(*************************************************************************)*/

expr:
 | xuexpr { }

 | expr TPlus expr { }
 | expr TMinus expr { }
 | expr TMul expr { }
 | expr TDiv expr { }
 | expr TMod expr { }

 | expr TAnd expr { }
 | expr TXor expr { }
 | expr TOr expr { }

 | expr TSupSup expr { }
 | expr TInfInf expr { }

 | expr TAndAnd expr { }
 | expr TOrOr expr { }

 | expr TEqEq expr { }
 | expr TBangEq expr { }

 | expr TInf expr { }
 | expr TSup expr  { }
 | expr TInfEq expr { }
 | expr TSupEq expr { }

 | expr TEq expr { }
 | expr TOpEq expr { }

 | expr TQuestion cexpr TColon expr { }

xuexpr:
 | uexpr { }

 | TOPar qualifier_and_type abdecor TCPar xuexpr  { }

uexpr:
 | pexpr { }

 | TPlus xuexpr { }
 | TMinus xuexpr { }

 | TBang xuexpr { }
 | TTilde xuexpr { }

 | TMul xuexpr { }
 | TAnd xuexpr  { }

 | TPlusPlus xuexpr { }
 | TMinusMinus xuexpr { } 

pexpr:
 | TOPar cexpr TCPar { }

 | pexpr TOPar zelist TCPar { }
 | pexpr TOBra cexpr TCBra { }

 | pexpr TDot tag { }
 | pexpr TArrow tag { } 

 | TName { }

 | TConst { } 
 | TFConst { }
 | string { }

 | pexpr TPlusPlus { }
 | pexpr TMinusMinus { } 

 | Tsizeof TOPar qualifier_and_type abdecor TCPar { }
 | Tsizeof uexpr { }

cexpr:
 | expr { }
 | cexpr TComma cexpr { }


string:
 | TString { }
 | string TString { }



zexpr:
 | /*(*empty*)*/ { }
 | lexpr { }

lexpr: expr { }

zcexpr:
 | /*(*empty*)*/ { }
 | cexpr { }

zelist:
 | /*(*empty*)*/ { }
 | elist { }

elist:
 | expr { }
 | elist TComma elist { }


/*(*************************************************************************)*/
/*(*1 Initializers *)*/
/*(*************************************************************************)*/

init: 
 | expr                  { ExprTodo }
 | TOBrace ilist TCBrace { ExprTodo }

ilist:
 | qlist { }
 | init { }
 | qlist init { }

qlist:
 | init TComma       { }
 | qlist init TComma { }
 | qual       { }
 | qlist qual { } 

qual:
 | TOBra lexpr TCBra { }
 | TDot tag { }
 | qual TEq { }


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

 | Tlong            { (Type.TLong Type.Signed) }
 | Tunsigned Tlong  { (Type.TLong Type.Unsigned) }

 | Tlong Tlong      { (Type.TVLong Type.Signed) }
 | Tunsigned Tlong Tlong { (Type.TVLong Type.Unsigned) }


 | Tfloat  { Type.TFloat }
 | Tdouble { Type.TDouble }

 | Tvoid   { Type.TVoid }

su:
 | Tstruct { Ast.Struct }
 | Tunion  { Ast.Union }

tag_opt:
 | tag          { $1 }
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
       let fullname = $2, env.block in
       Ast.TStructName ($1, fullname)
 }

 | su tag_opt sbody {
     let fullname = $2, env.block in
     (* todo: check if already defined? or conflicting su? *)
     defs := (StructDef { s_name = fullname; s_kind = $1; s_flds = $3 })::!defs;
     Ast.TStructName ($1, fullname)
 }


 | Tenum tag   { raise Todo }
 | Tenum tag_opt TOBrace enum TCBrace {
     let fullname = $2, env.block in
     defs := (EnumDef { e_name = fullname; e_constants = $4 })::!defs;
     Ast.TEnumName fullname
 }

 | TTypeName 
     { 
       try let (_,bid) = Hashtbl.find env.ids $1 in Ast.TTypeName ($1,bid) 
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
 * Pointer (Func(int,int)).
 *)*/

xdecor:
 | xdecor2                { $1 }
 | TMul qualifiers xdecor { $3 (* TODO *) }

/*(* use tag here too, as can have foo foo; declarations *)*/
xdecor2:
 | tag { $1, (fun x -> x) }
 | TOPar xdecor TCPar { $2 }
 | xdecor2 TOBra zexpr TCBra { $1  (* TODO *) }
 | xdecor2 TOPar zarglist TCPar { $1  (* TODO *) }




zarglist:
 | /*(*empty*)*/ { }
 | arglist { }

/*less: name { } */
arglist:
 | qualifier_and_type xdecor { }
 | qualifier_and_type abdecor { }

 | arglist TComma arglist { }
 | TDot TDot TDot { }


abdecor:
 | /*(*empty*)*/ { }
 | abdecor1      { }

abdecor1:
 | TMul qualifiers { }
 | TMul qualifiers abdecor1 { }
 | abdecor2  { }

abdecor2:
 | abdecor3 { }
 | abdecor2 TOPar zarglist TCPar { }
 | abdecor2 TOBra zexpr TCBra { }

abdecor3:
 | TOPar TCPar { }
 | TOBra zexpr TCBra { }
 | TOPar abdecor1 TCPar { }


/*(*************************************************************************)*/
/*(*1 Struct/union/enum definition *)*/
/*(*************************************************************************)*/

sbody: TOBrace edecl TCBrace { $2 }

edecl:
 |       edecl_elem TSemicolon { $1 }
 | edecl edecl_elem TSemicolon { $1 @ $2 }

edecl_elem: qualifier_and_type zedlist
 { $2 |> List.map (fun (id, typ2) -> 
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

/*(* todo: same scope than identifier? so modify htypedef? 
   * todo: populate early, as const_expr can reference enum constants defined before
   *)*/
enum:
 | TName                { [($1, env.block), None] }
 | TName TEq const_expr { [($1, env.block), Some $3] }

 | enum TComma enum { $1 @ $3 }
 | enum TComma      { $1 }

const_expr: expr { ExprTodo }

/*(*************************************************************************)*/
/*(*1 Storage, qualifiers *)*/
/*(*************************************************************************)*/

storage:
 | Tauto     { Storage.Auto }
 | Tstatic   { Storage.Static }
 | Textern   { Storage.Extern }
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
 | /*(*empty*)*/ { }
 | qualifiers qualifier { }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/
