%{
(* Copyright 2016 Yoann Padioleau, see copyright.txt *)
open Common

open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Limitations compared to 5c:
 *  - no support for old style parameter declaration
 *    (obsolete practice anyway)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: automatic lineno
let mk_e 
*)

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
%token <string> TOpEq

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

%left   TSemicolon
%left   TComma
%right  TEq TOpEq
%right  TQuestion TColon
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
prog:
 |  /*(*empty*)*/ { }
 | prog xdecl { }


/*(*************************************************************************)*/
/*(*1 Declarations *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 External declarators *)*/
/*(*-----------------------------------------*)*/

xdecl: 
 | zctlist TSemicolon { }
 | zctlist xdlist TSemicolon { }
 | zctlist xdecor block { }

zctlist:
 | /*(*empty*)*/ { }
 | ctllist { }

ctllist: types { }


xdlist:
 | xdecor { }
 | xdecor TEq init { }

 | xdlist TComma xdlist { }


/*(*-----------------------------------------*)*/
/*(*2 Automatic declarators *)*/
/*(*-----------------------------------------*)*/

adecl:
 | ctllist TSemicolon { }
 | ctllist adlist TSemicolon { }

/*(* equal to xdlist but different codegen *)*/
adlist:
 | xdecor { }
 | xdecor TEq init { }

 | adlist TComma adlist { }


/*(*************************************************************************)*/
/*(*1 Statements *)*/
/*(*************************************************************************)*/

block: TOBrace slist TCBrace { }

slist:
 | /*(*empty*)*/ { }
 | slist adecl { }
 | slist stmnt { }

stmnt: 
 | ulstmnt { }
 | labels ulstmnt { }
 | error TSemicolon { }

ulstmnt:
 | zcexpr TSemicolon { }
 | block { }
 | Tif TOPar cexpr TCPar stmnt { }
 | Tif TOPar cexpr TCPar stmnt Telse stmnt { }
 | Twhile TOPar cexpr TCPar stmnt { }
 | Tdo stmnt Twhile TOPar cexpr TCPar TSemicolon { }
 | Tfor TOPar forexpr TSemicolon zcexpr TSemicolon zcexpr TCPar stmnt { }
 | Treturn zcexpr TSemicolon { }
 | Tbreak TSemicolon { }
 | Tcontinue TSemicolon { }
 | Tswitch TOPar cexpr TCPar stmnt { }
 | Tgoto ltag TSemicolon { }



forexpr: 
 | zcexpr { }
 | ctllist adlist { }

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

 | TOPar tlist abdecor TCPar xuexpr  { }

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

 | pexpr TDot ltag { }
 | pexpr TArrow ltag { } 

 | name { }

 | TConst { } 
 | TFConst { }
 | string { }

 | pexpr TPlusPlus { }
 | pexpr TMinusMinus { } 

 | Tsizeof TOPar tlist abdecor TCPar { }
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
 | expr { }
 | TOBrace ilist TCBrace { }

ilist:
 | qlist { }
 | init { }
 | qlist init { }

qlist:
 | init TComma { }
 | qlist init TComma { }
 | qual { }
 | qlist qual { } 

qual:
 | TOBra lexpr TCBra { }
 | TDot ltag { }
 | qual TEq { }


/*(*************************************************************************)*/
/*(*1 Types *)*/
/*(*************************************************************************)*/

/*(*-----------------------------------------*)*/
/*(*2 Types part 1 *)*/
/*(*-----------------------------------------*)*/

tname:
 | Tchar { }
 | Tshort { }
 | Tint { }
 | Tlong { }

 | Tfloat { }
 | Tdouble { }

 | Tvoid { }

 | Tsigned { }
 | Tunsigned { }

complex:
 | Tstruct ltag { }
 | Tunion ltag { }
 | Tenum ltag { }

 | Tstruct ltag sbody { }
 | Tunion ltag sbody { }
 | Tenum ltag TOBrace enum TCBrace { }

 | Tstruct sbody { }
 | Tunion sbody { }
 | Tenum TOBrace enum TCBrace { }

 | TTypeName { }


/*(*-----------------------------------------*)*/
/*(*2 Types part 2 *)*/
/*(*-----------------------------------------*)*/

xdecor:
 | xdecor2 { }
 | TMul zgnlist xdecor { }

xdecor2:
 | tag { }
 | TOPar xdecor TCPar { }
 | xdecor2 TOBra zexpr TCBra { }
 | xdecor2 TOPar zarglist TCPar { }

tag:
 | ltag { }

ltag: 
 | TName { }
 | TTypeName { }

zarglist:
 | /*(*empty*)*/ { }
 | arglist { }

/*less: name { } */
arglist:
 | tlist xdecor { }
 | tlist abdecor { }

 | arglist TComma arglist { }
 | TDot TDot TDot { }


abdecor:
 | /*(*empty*)*/ { }
 | abdecor1 { }

abdecor1:
 | TMul zgnlist { }
 | TMul zgnlist abdecor1 { }
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

sbody: TOBrace edecl TCBrace { }

edecl:
 |       tlist zedlist TSemicolon { }
 | edecl tlist zedlist TSemicolon { }

zedlist:
 | /*(*empty*)*/ { }
 | edlist { }

edlist:
 | edecor { }
 | edlist TComma edecor { }

edecor:
 | xdecor { }


enum:
 | TName { }
 | TName TEq expr { }

 | enum TComma enum { }
 | enum TComma { }

/*(*************************************************************************)*/
/*(*1 Storage, qualifiers *)*/
/*(*************************************************************************)*/

gctname:
 | tname { }
 | gname { }
 | cname { }

gcname:
 | gname { }
 | cname { }


cname:
 | Tauto { }
 | Tstatic { }
 | Textern { }
 | Tregister { }
 | Tinline { }

 | Ttypedef { }

gname:
 | Tconst { }
 | Tvolatile { }
 | Trestrict { }

gctnlist: 
 | gctname { }
 | gctnlist gctname { }

gcnlist:
 | gcname { }
 | gcnlist gcname { }

types:
 | tname { }
 | gcnlist { }
 | tname gctnlist { }
 | gcnlist tname { }
 | gcnlist tname gctnlist { }

 | complex { }
 | complex gctnlist { }
 | gcnlist complex zgnlist { }

tlist: types { }

zgnlist:
 | /*(*empty*)*/ { }
 | zgnlist gname { }


/*(*************************************************************************)*/
/*(*1 Name *)*/
/*(*************************************************************************)*/

name:
 | TName { }

/*(*************************************************************************)*/
/*(*1 xxx_opt, xxx_list *)*/
/*(*************************************************************************)*/
