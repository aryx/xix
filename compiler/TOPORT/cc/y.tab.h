/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    LPE = 258,                     /* LPE  */
    LME = 259,                     /* LME  */
    LMLE = 260,                    /* LMLE  */
    LDVE = 261,                    /* LDVE  */
    LMDE = 262,                    /* LMDE  */
    LRSHE = 263,                   /* LRSHE  */
    LLSHE = 264,                   /* LLSHE  */
    LANDE = 265,                   /* LANDE  */
    LXORE = 266,                   /* LXORE  */
    LORE = 267,                    /* LORE  */
    LOROR = 268,                   /* LOROR  */
    LANDAND = 269,                 /* LANDAND  */
    LEQ = 270,                     /* LEQ  */
    LNE = 271,                     /* LNE  */
    LLE = 272,                     /* LLE  */
    LGE = 273,                     /* LGE  */
    LLSH = 274,                    /* LLSH  */
    LRSH = 275,                    /* LRSH  */
    LMM = 276,                     /* LMM  */
    LPP = 277,                     /* LPP  */
    LMG = 278,                     /* LMG  */
    LNAME = 279,                   /* LNAME  */
    LTYPE = 280,                   /* LTYPE  */
    LFCONST = 281,                 /* LFCONST  */
    LDCONST = 282,                 /* LDCONST  */
    LCONST = 283,                  /* LCONST  */
    LLCONST = 284,                 /* LLCONST  */
    LUCONST = 285,                 /* LUCONST  */
    LULCONST = 286,                /* LULCONST  */
    LVLCONST = 287,                /* LVLCONST  */
    LUVLCONST = 288,               /* LUVLCONST  */
    LSTRING = 289,                 /* LSTRING  */
    LLSTRING = 290,                /* LLSTRING  */
    LAUTO = 291,                   /* LAUTO  */
    LBREAK = 292,                  /* LBREAK  */
    LCASE = 293,                   /* LCASE  */
    LCHAR = 294,                   /* LCHAR  */
    LCONTINUE = 295,               /* LCONTINUE  */
    LDEFAULT = 296,                /* LDEFAULT  */
    LDO = 297,                     /* LDO  */
    LDOUBLE = 298,                 /* LDOUBLE  */
    LELSE = 299,                   /* LELSE  */
    LEXTERN = 300,                 /* LEXTERN  */
    LFLOAT = 301,                  /* LFLOAT  */
    LFOR = 302,                    /* LFOR  */
    LGOTO = 303,                   /* LGOTO  */
    LIF = 304,                     /* LIF  */
    LINT = 305,                    /* LINT  */
    LLONG = 306,                   /* LLONG  */
    LREGISTER = 307,               /* LREGISTER  */
    LRETURN = 308,                 /* LRETURN  */
    LSHORT = 309,                  /* LSHORT  */
    LSIZEOF = 310,                 /* LSIZEOF  */
    LUSED = 311,                   /* LUSED  */
    LSTATIC = 312,                 /* LSTATIC  */
    LSTRUCT = 313,                 /* LSTRUCT  */
    LSWITCH = 314,                 /* LSWITCH  */
    LTYPEDEF = 315,                /* LTYPEDEF  */
    LTYPESTR = 316,                /* LTYPESTR  */
    LUNION = 317,                  /* LUNION  */
    LUNSIGNED = 318,               /* LUNSIGNED  */
    LWHILE = 319,                  /* LWHILE  */
    LVOID = 320,                   /* LVOID  */
    LENUM = 321,                   /* LENUM  */
    LSIGNED = 322,                 /* LSIGNED  */
    LCONSTNT = 323,                /* LCONSTNT  */
    LVOLATILE = 324,               /* LVOLATILE  */
    LSET = 325,                    /* LSET  */
    LSIGNOF = 326,                 /* LSIGNOF  */
    LRESTRICT = 327,               /* LRESTRICT  */
    LINLINE = 328                  /* LINLINE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define LPE 258
#define LME 259
#define LMLE 260
#define LDVE 261
#define LMDE 262
#define LRSHE 263
#define LLSHE 264
#define LANDE 265
#define LXORE 266
#define LORE 267
#define LOROR 268
#define LANDAND 269
#define LEQ 270
#define LNE 271
#define LLE 272
#define LGE 273
#define LLSH 274
#define LRSH 275
#define LMM 276
#define LPP 277
#define LMG 278
#define LNAME 279
#define LTYPE 280
#define LFCONST 281
#define LDCONST 282
#define LCONST 283
#define LLCONST 284
#define LUCONST 285
#define LULCONST 286
#define LVLCONST 287
#define LUVLCONST 288
#define LSTRING 289
#define LLSTRING 290
#define LAUTO 291
#define LBREAK 292
#define LCASE 293
#define LCHAR 294
#define LCONTINUE 295
#define LDEFAULT 296
#define LDO 297
#define LDOUBLE 298
#define LELSE 299
#define LEXTERN 300
#define LFLOAT 301
#define LFOR 302
#define LGOTO 303
#define LIF 304
#define LINT 305
#define LLONG 306
#define LREGISTER 307
#define LRETURN 308
#define LSHORT 309
#define LSIZEOF 310
#define LUSED 311
#define LSTATIC 312
#define LSTRUCT 313
#define LSWITCH 314
#define LTYPEDEF 315
#define LTYPESTR 316
#define LUNION 317
#define LUNSIGNED 318
#define LWHILE 319
#define LVOID 320
#define LENUM 321
#define LSIGNED 322
#define LCONSTNT 323
#define LVOLATILE 324
#define LSET 325
#define LSIGNOF 326
#define LRESTRICT 327
#define LINLINE 328

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 35 "cc.y"

	Node*	node;
	Sym*	sym;
	Type*	type;
	struct
	{
		Type*	t;
		uchar	c;
	} tycl;
	struct
	{
		Type*	t1;
		Type*	t2;
		Type*	t3;
		uchar	c;
	} tyty;
	struct
	{
		char*	s;
		int32	l;
	} sval;
	int32	lval;
	double	dval;
	vlong	vval;

#line 239 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
