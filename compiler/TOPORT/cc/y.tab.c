/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 31 "cc.y"

#include <stdio.h>	/* if we don't, bison will, and cc.h re-#defines getc */
#include "cc.h"

#line 76 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
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

#line 301 "y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_3_ = 3,                         /* ';'  */
  YYSYMBOL_4_ = 4,                         /* ','  */
  YYSYMBOL_5_ = 5,                         /* '='  */
  YYSYMBOL_LPE = 6,                        /* LPE  */
  YYSYMBOL_LME = 7,                        /* LME  */
  YYSYMBOL_LMLE = 8,                       /* LMLE  */
  YYSYMBOL_LDVE = 9,                       /* LDVE  */
  YYSYMBOL_LMDE = 10,                      /* LMDE  */
  YYSYMBOL_LRSHE = 11,                     /* LRSHE  */
  YYSYMBOL_LLSHE = 12,                     /* LLSHE  */
  YYSYMBOL_LANDE = 13,                     /* LANDE  */
  YYSYMBOL_LXORE = 14,                     /* LXORE  */
  YYSYMBOL_LORE = 15,                      /* LORE  */
  YYSYMBOL_16_ = 16,                       /* '?'  */
  YYSYMBOL_17_ = 17,                       /* ':'  */
  YYSYMBOL_LOROR = 18,                     /* LOROR  */
  YYSYMBOL_LANDAND = 19,                   /* LANDAND  */
  YYSYMBOL_20_ = 20,                       /* '|'  */
  YYSYMBOL_21_ = 21,                       /* '^'  */
  YYSYMBOL_22_ = 22,                       /* '&'  */
  YYSYMBOL_LEQ = 23,                       /* LEQ  */
  YYSYMBOL_LNE = 24,                       /* LNE  */
  YYSYMBOL_25_ = 25,                       /* '<'  */
  YYSYMBOL_26_ = 26,                       /* '>'  */
  YYSYMBOL_LLE = 27,                       /* LLE  */
  YYSYMBOL_LGE = 28,                       /* LGE  */
  YYSYMBOL_LLSH = 29,                      /* LLSH  */
  YYSYMBOL_LRSH = 30,                      /* LRSH  */
  YYSYMBOL_31_ = 31,                       /* '+'  */
  YYSYMBOL_32_ = 32,                       /* '-'  */
  YYSYMBOL_33_ = 33,                       /* '*'  */
  YYSYMBOL_34_ = 34,                       /* '/'  */
  YYSYMBOL_35_ = 35,                       /* '%'  */
  YYSYMBOL_LMM = 36,                       /* LMM  */
  YYSYMBOL_LPP = 37,                       /* LPP  */
  YYSYMBOL_LMG = 38,                       /* LMG  */
  YYSYMBOL_39_ = 39,                       /* '.'  */
  YYSYMBOL_40_ = 40,                       /* '['  */
  YYSYMBOL_41_ = 41,                       /* '('  */
  YYSYMBOL_LNAME = 42,                     /* LNAME  */
  YYSYMBOL_LTYPE = 43,                     /* LTYPE  */
  YYSYMBOL_LFCONST = 44,                   /* LFCONST  */
  YYSYMBOL_LDCONST = 45,                   /* LDCONST  */
  YYSYMBOL_LCONST = 46,                    /* LCONST  */
  YYSYMBOL_LLCONST = 47,                   /* LLCONST  */
  YYSYMBOL_LUCONST = 48,                   /* LUCONST  */
  YYSYMBOL_LULCONST = 49,                  /* LULCONST  */
  YYSYMBOL_LVLCONST = 50,                  /* LVLCONST  */
  YYSYMBOL_LUVLCONST = 51,                 /* LUVLCONST  */
  YYSYMBOL_LSTRING = 52,                   /* LSTRING  */
  YYSYMBOL_LLSTRING = 53,                  /* LLSTRING  */
  YYSYMBOL_LAUTO = 54,                     /* LAUTO  */
  YYSYMBOL_LBREAK = 55,                    /* LBREAK  */
  YYSYMBOL_LCASE = 56,                     /* LCASE  */
  YYSYMBOL_LCHAR = 57,                     /* LCHAR  */
  YYSYMBOL_LCONTINUE = 58,                 /* LCONTINUE  */
  YYSYMBOL_LDEFAULT = 59,                  /* LDEFAULT  */
  YYSYMBOL_LDO = 60,                       /* LDO  */
  YYSYMBOL_LDOUBLE = 61,                   /* LDOUBLE  */
  YYSYMBOL_LELSE = 62,                     /* LELSE  */
  YYSYMBOL_LEXTERN = 63,                   /* LEXTERN  */
  YYSYMBOL_LFLOAT = 64,                    /* LFLOAT  */
  YYSYMBOL_LFOR = 65,                      /* LFOR  */
  YYSYMBOL_LGOTO = 66,                     /* LGOTO  */
  YYSYMBOL_LIF = 67,                       /* LIF  */
  YYSYMBOL_LINT = 68,                      /* LINT  */
  YYSYMBOL_LLONG = 69,                     /* LLONG  */
  YYSYMBOL_LREGISTER = 70,                 /* LREGISTER  */
  YYSYMBOL_LRETURN = 71,                   /* LRETURN  */
  YYSYMBOL_LSHORT = 72,                    /* LSHORT  */
  YYSYMBOL_LSIZEOF = 73,                   /* LSIZEOF  */
  YYSYMBOL_LUSED = 74,                     /* LUSED  */
  YYSYMBOL_LSTATIC = 75,                   /* LSTATIC  */
  YYSYMBOL_LSTRUCT = 76,                   /* LSTRUCT  */
  YYSYMBOL_LSWITCH = 77,                   /* LSWITCH  */
  YYSYMBOL_LTYPEDEF = 78,                  /* LTYPEDEF  */
  YYSYMBOL_LTYPESTR = 79,                  /* LTYPESTR  */
  YYSYMBOL_LUNION = 80,                    /* LUNION  */
  YYSYMBOL_LUNSIGNED = 81,                 /* LUNSIGNED  */
  YYSYMBOL_LWHILE = 82,                    /* LWHILE  */
  YYSYMBOL_LVOID = 83,                     /* LVOID  */
  YYSYMBOL_LENUM = 84,                     /* LENUM  */
  YYSYMBOL_LSIGNED = 85,                   /* LSIGNED  */
  YYSYMBOL_LCONSTNT = 86,                  /* LCONSTNT  */
  YYSYMBOL_LVOLATILE = 87,                 /* LVOLATILE  */
  YYSYMBOL_LSET = 88,                      /* LSET  */
  YYSYMBOL_LSIGNOF = 89,                   /* LSIGNOF  */
  YYSYMBOL_LRESTRICT = 90,                 /* LRESTRICT  */
  YYSYMBOL_LINLINE = 91,                   /* LINLINE  */
  YYSYMBOL_92_ = 92,                       /* ')'  */
  YYSYMBOL_93_ = 93,                       /* ']'  */
  YYSYMBOL_94_ = 94,                       /* '{'  */
  YYSYMBOL_95_ = 95,                       /* '}'  */
  YYSYMBOL_96_ = 96,                       /* '!'  */
  YYSYMBOL_97_ = 97,                       /* '~'  */
  YYSYMBOL_YYACCEPT = 98,                  /* $accept  */
  YYSYMBOL_prog = 99,                      /* prog  */
  YYSYMBOL_xdecl = 100,                    /* xdecl  */
  YYSYMBOL_101_1 = 101,                    /* $@1  */
  YYSYMBOL_102_2 = 102,                    /* $@2  */
  YYSYMBOL_xdlist = 103,                   /* xdlist  */
  YYSYMBOL_104_3 = 104,                    /* $@3  */
  YYSYMBOL_xdecor = 105,                   /* xdecor  */
  YYSYMBOL_xdecor2 = 106,                  /* xdecor2  */
  YYSYMBOL_adecl = 107,                    /* adecl  */
  YYSYMBOL_adlist = 108,                   /* adlist  */
  YYSYMBOL_109_4 = 109,                    /* $@4  */
  YYSYMBOL_pdecl = 110,                    /* pdecl  */
  YYSYMBOL_pdlist = 111,                   /* pdlist  */
  YYSYMBOL_edecl = 112,                    /* edecl  */
  YYSYMBOL_113_5 = 113,                    /* $@5  */
  YYSYMBOL_114_6 = 114,                    /* $@6  */
  YYSYMBOL_zedlist = 115,                  /* zedlist  */
  YYSYMBOL_edlist = 116,                   /* edlist  */
  YYSYMBOL_edecor = 117,                   /* edecor  */
  YYSYMBOL_abdecor = 118,                  /* abdecor  */
  YYSYMBOL_abdecor1 = 119,                 /* abdecor1  */
  YYSYMBOL_abdecor2 = 120,                 /* abdecor2  */
  YYSYMBOL_abdecor3 = 121,                 /* abdecor3  */
  YYSYMBOL_init = 122,                     /* init  */
  YYSYMBOL_qual = 123,                     /* qual  */
  YYSYMBOL_qlist = 124,                    /* qlist  */
  YYSYMBOL_ilist = 125,                    /* ilist  */
  YYSYMBOL_zarglist = 126,                 /* zarglist  */
  YYSYMBOL_arglist = 127,                  /* arglist  */
  YYSYMBOL_block = 128,                    /* block  */
  YYSYMBOL_slist = 129,                    /* slist  */
  YYSYMBOL_labels = 130,                   /* labels  */
  YYSYMBOL_label = 131,                    /* label  */
  YYSYMBOL_stmnt = 132,                    /* stmnt  */
  YYSYMBOL_forexpr = 133,                  /* forexpr  */
  YYSYMBOL_ulstmnt = 134,                  /* ulstmnt  */
  YYSYMBOL_135_7 = 135,                    /* $@7  */
  YYSYMBOL_136_8 = 136,                    /* $@8  */
  YYSYMBOL_zcexpr = 137,                   /* zcexpr  */
  YYSYMBOL_zexpr = 138,                    /* zexpr  */
  YYSYMBOL_lexpr = 139,                    /* lexpr  */
  YYSYMBOL_cexpr = 140,                    /* cexpr  */
  YYSYMBOL_expr = 141,                     /* expr  */
  YYSYMBOL_xuexpr = 142,                   /* xuexpr  */
  YYSYMBOL_uexpr = 143,                    /* uexpr  */
  YYSYMBOL_pexpr = 144,                    /* pexpr  */
  YYSYMBOL_string = 145,                   /* string  */
  YYSYMBOL_lstring = 146,                  /* lstring  */
  YYSYMBOL_zelist = 147,                   /* zelist  */
  YYSYMBOL_elist = 148,                    /* elist  */
  YYSYMBOL_sbody = 149,                    /* sbody  */
  YYSYMBOL_150_9 = 150,                    /* @9  */
  YYSYMBOL_zctlist = 151,                  /* zctlist  */
  YYSYMBOL_types = 152,                    /* types  */
  YYSYMBOL_tlist = 153,                    /* tlist  */
  YYSYMBOL_ctlist = 154,                   /* ctlist  */
  YYSYMBOL_complex = 155,                  /* complex  */
  YYSYMBOL_156_10 = 156,                   /* $@10  */
  YYSYMBOL_157_11 = 157,                   /* $@11  */
  YYSYMBOL_158_12 = 158,                   /* $@12  */
  YYSYMBOL_159_13 = 159,                   /* $@13  */
  YYSYMBOL_160_14 = 160,                   /* $@14  */
  YYSYMBOL_gctnlist = 161,                 /* gctnlist  */
  YYSYMBOL_zgnlist = 162,                  /* zgnlist  */
  YYSYMBOL_gctname = 163,                  /* gctname  */
  YYSYMBOL_gcnlist = 164,                  /* gcnlist  */
  YYSYMBOL_gcname = 165,                   /* gcname  */
  YYSYMBOL_enum = 166,                     /* enum  */
  YYSYMBOL_tname = 167,                    /* tname  */
  YYSYMBOL_cname = 168,                    /* cname  */
  YYSYMBOL_gname = 169,                    /* gname  */
  YYSYMBOL_name = 170,                     /* name  */
  YYSYMBOL_tag = 171,                      /* tag  */
  YYSYMBOL_ltag = 172                      /* ltag  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1202

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  98
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  75
/* YYNRULES -- Number of rules.  */
#define YYNRULES  246
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  412

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   328


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    96,     2,     2,     2,    35,    22,     2,
      41,    92,    33,    31,     4,    32,    39,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    17,     3,
      25,     5,    26,    16,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    40,     2,    93,    21,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    94,    20,    95,    97,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    18,    19,
      23,    24,    27,    28,    29,    30,    36,    37,    38,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   100,   100,   101,   107,   111,   113,   127,   112,   142,
     147,   146,   154,   157,   158,   165,   166,   170,   174,   183,
     187,   193,   199,   198,   210,   223,   224,   227,   231,   238,
     237,   243,   242,   249,   253,   256,   260,   263,   268,   272,
     281,   284,   287,   292,   297,   300,   301,   305,   311,   315,
     319,   325,   326,   332,   336,   341,   344,   345,   349,   350,
     356,   357,   358,   364,   367,   374,   375,   380,   385,   389,
     395,   405,   408,   412,   418,   419,   425,   429,   433,   439,
     443,   444,   450,   451,   457,   458,   458,   469,   475,   483,
     483,   494,   498,   502,   507,   521,   525,   529,   533,   537,
     543,   546,   549,   552,   555,   562,   563,   569,   570,   574,
     578,   582,   586,   590,   594,   598,   602,   606,   610,   614,
     618,   622,   626,   630,   634,   638,   642,   646,   650,   654,
     658,   662,   666,   670,   674,   678,   682,   686,   692,   693,
     700,   708,   709,   713,   717,   721,   725,   729,   733,   737,
     741,   745,   751,   755,   761,   767,   775,   779,   784,   789,
     793,   797,   798,   805,   812,   819,   826,   833,   840,   847,
     854,   855,   858,   868,   886,   896,   914,   917,   920,   921,
     928,   927,   950,   954,   957,   962,   967,   973,   981,   987,
     993,   999,  1007,  1015,  1022,  1028,  1027,  1039,  1047,  1053,
    1052,  1064,  1072,  1081,  1085,  1080,  1102,  1101,  1110,  1116,
    1117,  1123,  1126,  1132,  1133,  1134,  1137,  1138,  1144,  1145,
    1148,  1152,  1156,  1157,  1160,  1161,  1162,  1163,  1164,  1165,
    1166,  1167,  1168,  1171,  1172,  1173,  1174,  1175,  1176,  1177,
    1180,  1181,  1182,  1185,  1200,  1212,  1213
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "';'", "','", "'='",
  "LPE", "LME", "LMLE", "LDVE", "LMDE", "LRSHE", "LLSHE", "LANDE", "LXORE",
  "LORE", "'?'", "':'", "LOROR", "LANDAND", "'|'", "'^'", "'&'", "LEQ",
  "LNE", "'<'", "'>'", "LLE", "LGE", "LLSH", "LRSH", "'+'", "'-'", "'*'",
  "'/'", "'%'", "LMM", "LPP", "LMG", "'.'", "'['", "'('", "LNAME", "LTYPE",
  "LFCONST", "LDCONST", "LCONST", "LLCONST", "LUCONST", "LULCONST",
  "LVLCONST", "LUVLCONST", "LSTRING", "LLSTRING", "LAUTO", "LBREAK",
  "LCASE", "LCHAR", "LCONTINUE", "LDEFAULT", "LDO", "LDOUBLE", "LELSE",
  "LEXTERN", "LFLOAT", "LFOR", "LGOTO", "LIF", "LINT", "LLONG",
  "LREGISTER", "LRETURN", "LSHORT", "LSIZEOF", "LUSED", "LSTATIC",
  "LSTRUCT", "LSWITCH", "LTYPEDEF", "LTYPESTR", "LUNION", "LUNSIGNED",
  "LWHILE", "LVOID", "LENUM", "LSIGNED", "LCONSTNT", "LVOLATILE", "LSET",
  "LSIGNOF", "LRESTRICT", "LINLINE", "')'", "']'", "'{'", "'}'", "'!'",
  "'~'", "$accept", "prog", "xdecl", "$@1", "$@2", "xdlist", "$@3",
  "xdecor", "xdecor2", "adecl", "adlist", "$@4", "pdecl", "pdlist",
  "edecl", "$@5", "$@6", "zedlist", "edlist", "edecor", "abdecor",
  "abdecor1", "abdecor2", "abdecor3", "init", "qual", "qlist", "ilist",
  "zarglist", "arglist", "block", "slist", "labels", "label", "stmnt",
  "forexpr", "ulstmnt", "$@7", "$@8", "zcexpr", "zexpr", "lexpr", "cexpr",
  "expr", "xuexpr", "uexpr", "pexpr", "string", "lstring", "zelist",
  "elist", "sbody", "@9", "zctlist", "types", "tlist", "ctlist", "complex",
  "$@10", "$@11", "$@12", "$@13", "$@14", "gctnlist", "zgnlist", "gctname",
  "gcnlist", "gcname", "enum", "tname", "cname", "gname", "name", "tag",
  "ltag", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-337)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-204)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -337,   541,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,
    -337,  -337,  -337,  -337,   -19,  -337,  -337,   -19,  -337,  -337,
      29,  -337,  -337,  -337,  -337,  -337,  -337,   294,  -337,  -337,
     963,   928,  -337,   963,  -337,  -337,  -337,  -337,  -337,  -337,
     -77,  -337,   -54,  -337,   -28,  -337,  -337,   127,    45,   280,
      36,  -337,  -337,   963,  -337,  -337,  -337,  -337,  -337,  -337,
     963,   963,   928,    -8,    -8,    54,    61,   236,    67,  -337,
     127,  -337,   158,   745,   850,  -337,     2,   963,   889,  -337,
    -337,  -337,  -337,   166,     3,  -337,  -337,  -337,  -337,  -337,
     193,   928,   168,   745,   745,   745,   745,   745,   745,   607,
    -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,  -337,
    -337,   778,   813,   745,   745,    68,  -337,  1081,  -337,  -337,
     697,   121,   178,  -337,   172,   141,   231,   124,  -337,  -337,
    -337,   308,   745,    54,  -337,    54,   143,   127,   677,  -337,
    1081,  -337,  -337,  -337,  -337,  -337,  -337,    52,  1081,   156,
     607,  -337,   607,  -337,  -337,  -337,  -337,   745,   745,   745,
     745,   745,   745,   745,   745,   745,   745,   745,   745,   745,
     745,   745,   745,   745,   745,   745,   745,   745,   745,   745,
     745,   745,   745,   745,   745,   745,   745,  -337,  -337,   145,
     145,   745,   745,  -337,  -337,   205,  -337,   850,  -337,   745,
     142,  -337,  -337,  -337,   151,  -337,   308,   745,  -337,   235,
     242,  -337,   237,  1081,  -337,    10,  -337,  -337,  -337,   199,
     145,   745,   244,   250,   677,   163,   745,  -337,  -337,   -14,
     169,   156,   156,  1081,  1081,  1081,  1081,  1081,  1081,  1081,
    1081,  1081,  1081,  1081,    42,  1098,  1114,  1129,  1143,  1156,
    1167,  1167,   428,   428,   428,   428,   194,   194,   285,   285,
    -337,  -337,  -337,  -337,  -337,    11,  1081,   171,   262,  -337,
    -337,   209,   167,  -337,   175,   745,   850,   269,  -337,  -337,
     308,   745,  -337,   337,  -337,   127,  -337,   180,  -337,  -337,
     272,   250,  -337,  -337,   153,   710,   188,   189,   745,  -337,
    -337,   745,  -337,  -337,  -337,   195,   200,  -337,  -337,  -337,
     288,   277,   299,   745,   300,   289,   434,   145,   266,   745,
     271,   274,   283,   290,  -337,  -337,   503,  -337,  -337,  -337,
     143,   248,   327,   328,   301,  -337,  -337,  -337,   677,  -337,
    -337,  -337,   420,  -337,  -337,  -337,  -337,  -337,  -337,  1050,
    -337,  -337,   257,   343,   745,   345,   745,   745,   745,   745,
    -337,  -337,  -337,   311,  -337,  -337,   348,   203,   259,  -337,
     315,  -337,    57,  -337,   265,    60,    64,   270,   607,   353,
    -337,   127,  -337,   745,   434,   357,   434,   434,   358,   360,
    -337,   127,   168,  -337,    66,   302,  -337,  -337,  -337,  -337,
     745,   362,  -337,   364,   434,   368,  -337,  -337,   745,   284,
     434,  -337
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,   182,     1,   208,   233,   224,   231,   235,   230,   226,
     227,   238,   225,   234,     0,   236,   237,     0,   229,   232,
       0,   228,   240,   241,   242,   239,     3,     0,   193,   183,
     184,   186,   216,   185,   219,   218,   245,   246,   180,   197,
     194,   201,   198,   206,   202,     4,   211,     0,     0,     6,
      13,    15,   244,   187,   209,   213,   215,   214,   211,   217,
     190,   188,     0,     0,     0,     0,     0,     0,     0,     5,
       0,    25,     0,   102,    63,   210,   189,   191,     0,   192,
      29,   196,   200,   220,     0,   204,    14,   212,    16,    12,
       9,     7,     0,     0,     0,     0,     0,     0,     0,     0,
     243,   167,   166,   162,   163,   164,   165,   168,   169,   172,
     174,     0,     0,     0,     0,     0,   103,   104,   107,   138,
     141,   170,   171,   161,     0,     0,    64,    40,    65,   181,
      31,    33,     0,   222,   207,     0,     0,     0,     0,    11,
      51,   143,   144,   145,   142,   149,   148,     0,   105,    40,
       0,   150,     0,   151,   146,   147,    18,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   160,   159,     0,
       0,     0,   176,   173,   175,     0,    17,     0,   211,   102,
       0,    67,    66,    41,    44,    45,    33,     0,    37,     0,
      34,    35,    15,   221,   223,     0,    71,     8,    27,     0,
       0,     0,    61,    58,    60,     0,     0,   152,   211,     0,
       0,    40,    40,   127,   128,   129,   130,   131,   132,   134,
     133,   135,   136,   137,     0,   125,   124,   123,   122,   121,
     119,   120,   115,   116,   117,   118,   114,   113,   111,   112,
     108,   109,   110,   157,   158,     0,   178,     0,   177,    68,
      69,    42,     0,    48,     0,   102,    63,     0,    39,    30,
       0,     0,   205,     0,    26,     0,    54,     0,    56,    55,
      62,    59,    52,   106,    42,     0,     0,     0,     0,   156,
     155,     0,    43,    49,    50,     0,     0,    32,    36,    38,
       0,   243,     0,     0,     0,     0,     0,     0,     0,   100,
       0,     0,     0,     0,    70,    72,    85,    74,    73,    80,
       0,     0,     0,   101,     0,    28,    53,    57,     0,   139,
     153,   154,   126,   179,    47,    46,    79,    78,    95,     0,
      96,    77,     0,     0,     0,     0,   176,     0,     0,   176,
      75,    81,    86,     0,    84,    19,    21,     0,     0,    76,
       0,    97,     0,    93,     0,     0,     0,     0,   100,     0,
      20,     0,   140,     0,     0,     0,     0,     0,     0,     0,
      82,     0,     0,    24,     0,    87,    98,    94,    91,    99,
     100,    83,    23,     0,     0,     0,    92,    88,   100,     0,
       0,    90
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -337,  -337,  -337,  -337,  -337,   305,  -337,   -26,  -337,  -337,
    -336,  -337,  -337,    87,  -337,  -337,  -337,   223,   119,  -337,
    -129,  -187,  -337,  -337,   -82,   206,  -337,   126,   192,   275,
     139,  -337,  -337,   147,  -304,  -337,   148,  -337,  -337,  -228,
    -181,  -182,   -83,   -45,   -63,   111,  -337,  -337,  -337,  -302,
     176,    46,  -337,  -337,    -1,    -4,   -88,   457,  -337,  -337,
    -337,  -337,  -337,     5,   -47,    20,  -337,   460,   -73,   256,
     268,   -24,   -52,  -127,   -12
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,    26,    71,   136,    48,    72,   208,    50,   325,
     367,   379,    91,   219,    78,   131,   206,   209,   210,   211,
     202,   203,   204,   205,   222,   223,   224,   225,   125,   126,
     217,   283,   326,   327,   328,   389,   329,   330,   331,   332,
     115,   116,   333,   148,   118,   119,   120,   121,   122,   267,
     268,    39,    62,    27,    79,   127,    29,    30,    63,    64,
      66,   135,    65,    53,    67,    54,    31,    32,    84,    33,
      34,    35,   123,    51,    52
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      28,    49,    40,   137,   212,    42,    57,   133,    44,    57,
     139,    76,   352,   274,   133,   226,   147,  -195,   272,   228,
     230,    68,   128,    36,    37,   278,   199,   229,   117,    57,
     141,   142,   143,   144,   145,   146,    57,    57,    61,   287,
    -199,    86,   274,    87,    90,   393,   226,   140,    69,    70,
     154,   155,    87,    57,   374,   401,   226,   377,    80,   298,
     214,   226,   215,    41,   226,    77,  -203,   147,   226,   147,
     226,    36,    37,    75,   130,    38,    73,    74,   273,   212,
     395,    75,   397,   398,   302,   244,    38,   213,    22,    23,
      28,   355,    24,   140,   305,   149,    83,    75,   134,   309,
     407,   201,   296,   297,   299,   282,   411,   302,   265,    81,
      82,   218,   233,   234,   235,   236,   237,   238,   239,   240,
     241,   242,   243,    43,   245,   246,   247,   248,   249,   250,
     251,   252,   253,   254,   255,   256,   257,   258,   259,   260,
     261,   262,   290,   293,   227,   128,   231,   266,   232,   384,
     390,   271,   386,   212,   117,    85,   387,   198,   403,    88,
      46,   156,   117,    92,   199,   200,    36,    37,    47,    36,
      37,   132,   405,   193,    68,   198,   117,   263,   264,   140,
     409,   294,   199,   200,    36,    37,   228,    36,    37,   228,
      93,   275,   276,   199,   229,   334,   199,   229,   -10,    94,
      95,    96,   284,   285,    97,    98,   380,   381,   286,    99,
     100,   195,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   151,   153,   128,   182,   183,   184,   185,   186,
     117,   194,   339,   196,   273,   197,   117,   216,   279,    22,
      23,   111,   198,    24,   269,    86,   280,    87,   288,   199,
     200,    36,    37,   342,   281,   289,   266,   112,   292,   218,
     303,   295,   138,   300,   113,   114,   301,   304,   349,    46,
      87,   372,   307,   336,   375,   376,   337,    47,    36,    37,
     340,   341,    28,    -9,    -9,   -10,    55,    60,   344,    55,
     391,   346,   345,   140,   347,    22,    23,    45,    56,    24,
     394,    56,   348,   350,   365,   353,   351,   354,   366,    55,
     402,   266,   356,   363,   266,   357,    55,    55,   184,   185,
     186,    56,    22,    23,   358,   207,    24,    46,    56,    56,
     364,   359,   226,    55,    46,    47,    36,    37,   310,   370,
    -100,    46,    47,    36,    37,    56,   371,   140,   373,    47,
      36,    37,   378,   -22,   382,   366,   383,   385,   392,    93,
     396,   399,   388,   400,   404,   366,   381,   406,    94,    95,
      96,   408,   335,    97,    98,    89,   410,    28,    99,   311,
       3,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,     4,   312,   313,     5,   314,   315,   316,     6,   308,
       7,     8,   -89,   317,   318,     9,    10,    11,   319,    12,
     111,   320,    13,    14,   321,    15,    16,    17,    18,   322,
      19,    20,    21,    22,    23,   323,   112,    24,    25,   277,
     291,   -85,   324,   113,   114,   310,   168,  -100,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,    93,   180,   181,   182,
     183,   184,   185,   186,   368,    94,    95,    96,   306,   362,
      97,    98,   270,   360,   361,    99,   311,   343,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,    58,   312,
     313,    59,   314,   315,   316,     0,     0,     0,     0,   -89,
     317,   318,     0,     0,     0,   319,  -100,   111,   320,     0,
       0,   321,     0,     0,     0,     0,   322,     0,     0,     0,
       0,     0,   323,   112,     0,    93,     0,     0,   -85,     0,
     113,   114,     0,     0,    94,    95,    96,     0,     0,    97,
      98,     2,     0,     0,    99,   311,     0,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,     0,   312,   313,
       0,   314,   315,   316,     0,     0,     0,     0,   -89,   317,
     318,     0,     0,     0,   319,     0,   111,   320,     0,     0,
     321,     0,     0,     0,     3,   322,     0,     0,     0,     0,
       0,   323,   112,     0,     0,     4,     0,     0,     5,   113,
     114,     0,     6,     0,     7,     8,     0,     0,     0,     9,
      10,    11,     0,    12,     0,     0,    13,    14,     0,    15,
      16,    17,    18,     0,    19,    20,    21,    22,    23,    93,
       0,    24,    25,     0,     0,     0,     0,     0,    94,    95,
      96,     0,     0,    97,    98,     0,     0,     0,    99,   100,
       3,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,     4,     0,     0,     5,     0,     0,     0,     6,     0,
       7,     8,     0,     0,     0,     9,    10,    11,     0,    12,
     111,     0,    13,    14,     0,    15,    16,    17,    18,     0,
      19,    20,    21,    22,    23,     0,   112,    24,    25,    93,
       0,     0,     0,   113,   114,     0,     0,     0,    94,    95,
      96,     0,     0,    97,    98,     0,   220,   221,    99,   100,
       0,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,     0,    93,   187,   188,   189,   190,   191,   192,     0,
       0,    94,    95,    96,     0,     0,    97,    98,     0,     0,
     111,    99,   100,     0,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,     0,     0,   112,    93,     0,     0,
       0,   138,     0,   113,   114,     0,    94,    95,    96,     0,
       0,    97,    98,   111,     0,     0,    99,   100,     0,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   112,
      93,     0,     0,     0,   338,     0,   113,   114,     0,    94,
      95,    96,     0,     0,    97,    98,     0,     0,   111,   150,
     100,     0,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,     0,     0,   112,    93,     0,     0,     0,     0,
       0,   113,   114,     0,    94,    95,    96,     0,     0,    97,
      98,   111,     0,     0,   152,   100,     0,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   112,     0,     0,
       0,     0,     0,     0,   113,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,   124,
       0,     0,   100,     3,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     4,     0,     0,     5,     0,   113,
     114,     6,     0,     7,     8,     0,     0,     0,     9,    10,
      11,     0,    12,     0,     0,    13,    14,     0,    15,    16,
      17,    18,     3,    19,    20,    21,    22,    23,     0,     0,
      24,    25,     0,     4,     0,     0,     5,     0,     0,     0,
       6,     0,     7,     8,     0,     0,     0,     9,    10,    11,
       0,    12,     0,     0,    13,    14,     0,    15,    16,    17,
      18,     3,    19,    20,    21,    22,    23,     0,     0,    24,
      25,     0,     4,     0,   129,     5,     0,     0,     0,     6,
       0,     7,     8,     0,     0,     0,     9,    10,    11,     0,
      12,     0,     0,    13,    14,     0,    15,    16,    17,    18,
       0,    19,    20,    21,    22,    23,     0,     4,    24,    25,
       5,     0,     0,     0,     6,     0,     7,     8,     0,     0,
       0,     9,    10,    11,     0,    12,     0,     0,    13,     0,
       0,    15,    16,     0,    18,     0,    19,     0,    21,    22,
      23,     0,     0,    24,    25,   157,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   369,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,     0,   169,
     170,   171,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   183,   184,   185,   186,   170,   171,   172,
     173,   174,   175,   176,   177,   178,   179,   180,   181,   182,
     183,   184,   185,   186,   171,   172,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,   183,   184,   185,   186,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   176,   177,   178,   179,   180,   181,   182,   183,
     184,   185,   186
};

static const yytype_int16 yycheck[] =
{
       1,    27,    14,    91,   131,    17,    30,     4,    20,    33,
      92,    58,   316,   200,     4,     4,    99,    94,   199,    33,
     149,    47,    74,    42,    43,   207,    40,    41,    73,    53,
      93,    94,    95,    96,    97,    98,    60,    61,    33,   221,
      94,    67,   229,    67,    70,   381,     4,    92,     3,     4,
     113,   114,    76,    77,   356,   391,     4,   359,    62,    17,
     133,     4,   135,    17,     4,    60,    94,   150,     4,   152,
       4,    42,    43,    53,    78,    94,    40,    41,    92,   206,
     384,    61,   386,   387,   271,   168,    94,   132,    86,    87,
      91,   319,    90,   138,   275,    99,    42,    77,    95,   281,
     404,   127,   231,   232,    93,    95,   410,   294,   191,    63,
      64,   137,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,    94,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   224,   226,    92,   197,   150,   192,   152,    92,
     378,   198,    92,   280,   199,    94,    92,    33,    92,    92,
      33,    93,   207,     5,    40,    41,    42,    43,    41,    42,
      43,     5,   400,    52,   200,    33,   221,   189,   190,   224,
     408,   228,    40,    41,    42,    43,    33,    42,    43,    33,
      22,    40,    41,    40,    41,   283,    40,    41,     5,    31,
      32,    33,     3,     4,    36,    37,     3,     4,   220,    41,
      42,    39,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,   111,   112,   276,    31,    32,    33,    34,    35,
     275,    53,   295,    92,    92,     4,   281,    94,     3,    86,
      87,    73,    33,    90,    39,   271,     4,   271,     4,    40,
      41,    42,    43,   298,    17,     5,   301,    89,    95,   285,
      93,    92,    94,    92,    96,    97,     4,    92,   313,    33,
     294,   354,     3,    93,   357,   358,     4,    41,    42,    43,
      92,    92,   283,     3,     4,     5,    30,    31,    93,    33,
     378,     3,    92,   338,    17,    86,    87,     3,    30,    90,
     383,    33,     3,     3,     3,   317,    17,    41,   334,    53,
     392,   356,    41,    65,   359,    41,    60,    61,    33,    34,
      35,    53,    86,    87,    41,    17,    90,    33,    60,    61,
       3,    41,     4,    77,    33,    41,    42,    43,     1,    82,
       3,    33,    41,    42,    43,    77,     3,   392,     3,    41,
      42,    43,    41,     5,    95,   381,    41,    92,     5,    22,
       3,     3,    92,     3,    62,   391,     4,     3,    31,    32,
      33,     3,   285,    36,    37,    70,    92,   378,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,   280,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,   206,
     224,    94,    95,    96,    97,     1,    16,     3,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    22,    29,    30,    31,
      32,    33,    34,    35,   338,    31,    32,    33,   276,   330,
      36,    37,   197,   326,   326,    41,    42,   301,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    31,    55,
      56,    31,    58,    59,    60,    -1,    -1,    -1,    -1,    65,
      66,    67,    -1,    -1,    -1,    71,     3,    73,    74,    -1,
      -1,    77,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    89,    -1,    22,    -1,    -1,    94,    -1,
      96,    97,    -1,    -1,    31,    32,    33,    -1,    -1,    36,
      37,     0,    -1,    -1,    41,    42,    -1,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    -1,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    -1,    -1,    65,    66,
      67,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,    -1,
      77,    -1,    -1,    -1,    43,    82,    -1,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    54,    -1,    -1,    57,    96,
      97,    -1,    61,    -1,    63,    64,    -1,    -1,    -1,    68,
      69,    70,    -1,    72,    -1,    -1,    75,    76,    -1,    78,
      79,    80,    81,    -1,    83,    84,    85,    86,    87,    22,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    -1,    -1,    36,    37,    -1,    -1,    -1,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    -1,    57,    -1,    -1,    -1,    61,    -1,
      63,    64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,
      73,    -1,    75,    76,    -1,    78,    79,    80,    81,    -1,
      83,    84,    85,    86,    87,    -1,    89,    90,    91,    22,
      -1,    -1,    -1,    96,    97,    -1,    -1,    -1,    31,    32,
      33,    -1,    -1,    36,    37,    -1,    39,    40,    41,    42,
      -1,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    -1,    22,    36,    37,    38,    39,    40,    41,    -1,
      -1,    31,    32,    33,    -1,    -1,    36,    37,    -1,    -1,
      73,    41,    42,    -1,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    -1,    -1,    89,    22,    -1,    -1,
      -1,    94,    -1,    96,    97,    -1,    31,    32,    33,    -1,
      -1,    36,    37,    73,    -1,    -1,    41,    42,    -1,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    89,
      22,    -1,    -1,    -1,    94,    -1,    96,    97,    -1,    31,
      32,    33,    -1,    -1,    36,    37,    -1,    -1,    73,    41,
      42,    -1,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    -1,    -1,    89,    22,    -1,    -1,    -1,    -1,
      -1,    96,    97,    -1,    31,    32,    33,    -1,    -1,    36,
      37,    73,    -1,    -1,    41,    42,    -1,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    96,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    39,
      -1,    -1,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    54,    -1,    -1,    57,    -1,    96,
      97,    61,    -1,    63,    64,    -1,    -1,    -1,    68,    69,
      70,    -1,    72,    -1,    -1,    75,    76,    -1,    78,    79,
      80,    81,    43,    83,    84,    85,    86,    87,    -1,    -1,
      90,    91,    -1,    54,    -1,    -1,    57,    -1,    -1,    -1,
      61,    -1,    63,    64,    -1,    -1,    -1,    68,    69,    70,
      -1,    72,    -1,    -1,    75,    76,    -1,    78,    79,    80,
      81,    43,    83,    84,    85,    86,    87,    -1,    -1,    90,
      91,    -1,    54,    -1,    95,    57,    -1,    -1,    -1,    61,
      -1,    63,    64,    -1,    -1,    -1,    68,    69,    70,    -1,
      72,    -1,    -1,    75,    76,    -1,    78,    79,    80,    81,
      -1,    83,    84,    85,    86,    87,    -1,    54,    90,    91,
      57,    -1,    -1,    -1,    61,    -1,    63,    64,    -1,    -1,
      -1,    68,    69,    70,    -1,    72,    -1,    -1,    75,    -1,
      -1,    78,    79,    -1,    81,    -1,    83,    -1,    85,    86,
      87,    -1,    -1,    90,    91,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    -1,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    99,     0,    43,    54,    57,    61,    63,    64,    68,
      69,    70,    72,    75,    76,    78,    79,    80,    81,    83,
      84,    85,    86,    87,    90,    91,   100,   151,   152,   154,
     155,   164,   165,   167,   168,   169,    42,    43,    94,   149,
     172,   149,   172,    94,   172,     3,    33,    41,   103,   105,
     106,   171,   172,   161,   163,   167,   168,   169,   155,   165,
     167,   161,   150,   156,   157,   160,   158,   162,   105,     3,
       4,   101,   104,    40,    41,   163,   162,   161,   112,   152,
     153,   149,   149,    42,   166,    94,   105,   169,    92,   103,
     105,   110,     5,    22,    31,    32,    33,    36,    37,    41,
      42,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    73,    89,    96,    97,   138,   139,   141,   142,   143,
     144,   145,   146,   170,    39,   126,   127,   153,   170,    95,
     153,   113,     5,     4,    95,   159,   102,   154,    94,   122,
     141,   142,   142,   142,   142,   142,   142,   140,   141,   153,
      41,   143,    41,   143,   142,   142,    93,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    52,    53,    39,    92,     4,    33,    40,
      41,   105,   118,   119,   120,   121,   114,    17,   105,   115,
     116,   117,   171,   141,   166,   166,    94,   128,   105,   111,
      39,    40,   122,   123,   124,   125,     4,    92,    33,    41,
     118,   153,   153,   141,   141,   141,   141,   141,   141,   141,
     141,   141,   141,   141,   140,   141,   141,   141,   141,   141,
     141,   141,   141,   141,   141,   141,   141,   141,   141,   141,
     141,   141,   141,   172,   172,   140,   141,   147,   148,    39,
     127,   162,   138,    92,   119,    40,    41,   115,   139,     3,
       4,    17,    95,   129,     3,     4,   172,   139,     4,     5,
     122,   123,    95,   140,   162,    92,   118,   118,    17,    93,
      92,     4,   119,    93,    92,   138,   126,     3,   116,   139,
       1,    42,    55,    56,    58,    59,    60,    66,    67,    71,
      74,    77,    82,    88,    95,   107,   130,   131,   132,   134,
     135,   136,   137,   140,   154,   111,    93,     4,    94,   142,
      92,    92,   141,   148,    93,    92,     3,    17,     3,   141,
       3,    17,   132,   172,    41,   137,    41,    41,    41,    41,
     131,   134,   128,    65,     3,     3,   105,   108,   125,    17,
      82,     3,   140,     3,   147,   140,   140,   147,    41,   109,
       3,     4,    95,    41,    92,    92,    92,    92,    92,   133,
     137,   154,     5,   108,   140,   132,     3,   132,   132,     3,
       3,   108,   122,    92,    62,   137,     3,   132,     3,   137,
      92,   132
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    98,    99,    99,   100,   100,   101,   102,   100,   103,
     104,   103,   103,   105,   105,   106,   106,   106,   106,   107,
     107,   108,   109,   108,   108,   110,   110,   111,   111,   113,
     112,   114,   112,   115,   115,   116,   116,   117,   117,   117,
     118,   118,   119,   119,   119,   120,   120,   120,   121,   121,
     121,   122,   122,   123,   123,   123,   124,   124,   124,   124,
     125,   125,   125,   126,   126,   127,   127,   127,   127,   127,
     128,   129,   129,   129,   130,   130,   131,   131,   131,   132,
     132,   132,   133,   133,   134,   135,   134,   134,   134,   136,
     134,   134,   134,   134,   134,   134,   134,   134,   134,   134,
     137,   137,   138,   138,   139,   140,   140,   141,   141,   141,
     141,   141,   141,   141,   141,   141,   141,   141,   141,   141,
     141,   141,   141,   141,   141,   141,   141,   141,   141,   141,
     141,   141,   141,   141,   141,   141,   141,   141,   142,   142,
     142,   143,   143,   143,   143,   143,   143,   143,   143,   143,
     143,   143,   144,   144,   144,   144,   144,   144,   144,   144,
     144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
     144,   144,   145,   145,   146,   146,   147,   147,   148,   148,
     150,   149,   151,   151,   152,   152,   152,   152,   152,   152,
     152,   152,   153,   154,   155,   156,   155,   155,   155,   157,
     155,   155,   155,   158,   159,   155,   160,   155,   155,   161,
     161,   162,   162,   163,   163,   163,   164,   164,   165,   165,
     166,   166,   166,   166,   167,   167,   167,   167,   167,   167,
     167,   167,   167,   168,   168,   168,   168,   168,   168,   168,
     169,   169,   169,   170,   171,   172,   172
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     3,     0,     0,     6,     1,
       0,     4,     3,     1,     3,     1,     3,     4,     4,     2,
       3,     1,     0,     4,     3,     0,     4,     1,     3,     0,
       4,     0,     5,     0,     1,     1,     3,     1,     3,     2,
       0,     1,     2,     3,     1,     1,     4,     4,     2,     3,
       3,     1,     3,     3,     2,     2,     2,     3,     1,     2,
       1,     1,     2,     0,     1,     1,     2,     2,     3,     3,
       3,     0,     2,     2,     1,     2,     3,     2,     2,     2,
       1,     2,     1,     2,     2,     0,     2,     5,     7,     0,
      10,     5,     7,     3,     5,     2,     2,     3,     5,     5,
       0,     1,     0,     1,     1,     1,     3,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     5,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     1,     5,
       7,     1,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     3,     5,     5,     4,     4,     3,     3,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     2,     0,     1,     1,     3,
       0,     4,     0,     1,     1,     1,     1,     2,     2,     3,
       2,     3,     1,     1,     2,     0,     4,     2,     2,     0,
       4,     2,     2,     0,     0,     7,     0,     5,     1,     1,
       2,     0,     2,     1,     1,     1,     1,     2,     1,     1,
       1,     3,     2,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 4: /* xdecl: zctlist ';'  */
#line 108 "cc.y"
        {
		dodecl(xdecl, lastclass, lasttype, Z);
	}
#line 1918 "y.tab.c"
    break;

  case 6: /* $@1: %empty  */
#line 113 "cc.y"
        {
		lastdcl = T;
		firstarg = S;
		dodecl(xdecl, lastclass, lasttype, (yyvsp[0].node));
		if(lastdcl == T || lastdcl->etype != TFUNC) {
			diag((yyvsp[0].node), "not a function");
			lastdcl = types[TFUNC];
		}
		thisfn = lastdcl;
		markdcl();
		firstdcl = dclstack;
		argmark((yyvsp[0].node), 0);
	}
#line 1936 "y.tab.c"
    break;

  case 7: /* $@2: %empty  */
#line 127 "cc.y"
        {
		argmark((yyvsp[-2].node), 1);
	}
#line 1944 "y.tab.c"
    break;

  case 8: /* xdecl: zctlist xdecor $@1 pdecl $@2 block  */
#line 131 "cc.y"
        {
		Node *n;

		n = revertdcl();
		if(n)
			(yyvsp[0].node) = new(OLIST, n, (yyvsp[0].node));
		if(!debug['a'] && !debug['Z'])
			codgen((yyvsp[0].node), (yyvsp[-4].node));
	}
#line 1958 "y.tab.c"
    break;

  case 9: /* xdlist: xdecor  */
#line 143 "cc.y"
        {
		dodecl(xdecl, lastclass, lasttype, (yyvsp[0].node));
	}
#line 1966 "y.tab.c"
    break;

  case 10: /* $@3: %empty  */
#line 147 "cc.y"
        {
		(yyvsp[0].node) = dodecl(xdecl, lastclass, lasttype, (yyvsp[0].node));
	}
#line 1974 "y.tab.c"
    break;

  case 11: /* xdlist: xdecor $@3 '=' init  */
#line 151 "cc.y"
        {
		doinit((yyvsp[-3].node)->sym, (yyvsp[-3].node)->type, 0L, (yyvsp[0].node));
	}
#line 1982 "y.tab.c"
    break;

  case 14: /* xdecor: '*' zgnlist xdecor  */
#line 159 "cc.y"
        {
		(yyval.node) = new(OIND, (yyvsp[0].node), Z);
		(yyval.node)->garb = simpleg((yyvsp[-1].lval));
	}
#line 1991 "y.tab.c"
    break;

  case 16: /* xdecor2: '(' xdecor ')'  */
#line 167 "cc.y"
        {
		(yyval.node) = (yyvsp[-1].node);
	}
#line 1999 "y.tab.c"
    break;

  case 17: /* xdecor2: xdecor2 '(' zarglist ')'  */
#line 171 "cc.y"
        {
		(yyval.node) = new(OFUNC, (yyvsp[-3].node), (yyvsp[-1].node));
	}
#line 2007 "y.tab.c"
    break;

  case 18: /* xdecor2: xdecor2 '[' zexpr ']'  */
#line 175 "cc.y"
        {
		(yyval.node) = new(OARRAY, (yyvsp[-3].node), (yyvsp[-1].node));
	}
#line 2015 "y.tab.c"
    break;

  case 19: /* adecl: ctlist ';'  */
#line 184 "cc.y"
        {
		(yyval.node) = dodecl(adecl, lastclass, lasttype, Z);
	}
#line 2023 "y.tab.c"
    break;

  case 20: /* adecl: ctlist adlist ';'  */
#line 188 "cc.y"
        {
		(yyval.node) = (yyvsp[-1].node);
	}
#line 2031 "y.tab.c"
    break;

  case 21: /* adlist: xdecor  */
#line 194 "cc.y"
        {
		dodecl(adecl, lastclass, lasttype, (yyvsp[0].node));
		(yyval.node) = Z;
	}
#line 2040 "y.tab.c"
    break;

  case 22: /* $@4: %empty  */
#line 199 "cc.y"
        {
		(yyvsp[0].node) = dodecl(adecl, lastclass, lasttype, (yyvsp[0].node));
	}
#line 2048 "y.tab.c"
    break;

  case 23: /* adlist: xdecor $@4 '=' init  */
#line 203 "cc.y"
        {
		int32 w;

		w = (yyvsp[-3].node)->sym->type->width;
		(yyval.node) = doinit((yyvsp[-3].node)->sym, (yyvsp[-3].node)->type, 0L, (yyvsp[0].node));
		(yyval.node) = contig((yyvsp[-3].node)->sym, (yyval.node), w);
	}
#line 2060 "y.tab.c"
    break;

  case 24: /* adlist: adlist ',' adlist  */
#line 211 "cc.y"
        {
		(yyval.node) = (yyvsp[-2].node);
		if((yyvsp[0].node) != Z) {
			(yyval.node) = (yyvsp[0].node);
			if((yyvsp[-2].node) != Z)
				(yyval.node) = new(OLIST, (yyvsp[-2].node), (yyvsp[0].node));
		}
	}
#line 2073 "y.tab.c"
    break;

  case 27: /* pdlist: xdecor  */
#line 228 "cc.y"
        {
		dodecl(pdecl, lastclass, lasttype, (yyvsp[0].node));
	}
#line 2081 "y.tab.c"
    break;

  case 29: /* $@5: %empty  */
#line 238 "cc.y"
        {
		lasttype = (yyvsp[0].type);
	}
#line 2089 "y.tab.c"
    break;

  case 31: /* $@6: %empty  */
#line 243 "cc.y"
        {
		lasttype = (yyvsp[0].type);
	}
#line 2097 "y.tab.c"
    break;

  case 33: /* zedlist: %empty  */
#line 249 "cc.y"
        {
		lastfield = 0;
		edecl(CXXX, lasttype, S);
	}
#line 2106 "y.tab.c"
    break;

  case 35: /* edlist: edecor  */
#line 257 "cc.y"
        {
		dodecl(edecl, CXXX, lasttype, (yyvsp[0].node));
	}
#line 2114 "y.tab.c"
    break;

  case 37: /* edecor: xdecor  */
#line 264 "cc.y"
        {
		lastbit = 0;
		firstbit = 1;
	}
#line 2123 "y.tab.c"
    break;

  case 38: /* edecor: tag ':' lexpr  */
#line 269 "cc.y"
        {
		(yyval.node) = new(OBIT, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2131 "y.tab.c"
    break;

  case 39: /* edecor: ':' lexpr  */
#line 273 "cc.y"
        {
		(yyval.node) = new(OBIT, Z, (yyvsp[0].node));
	}
#line 2139 "y.tab.c"
    break;

  case 40: /* abdecor: %empty  */
#line 281 "cc.y"
        {
		(yyval.node) = (Z);
	}
#line 2147 "y.tab.c"
    break;

  case 42: /* abdecor1: '*' zgnlist  */
#line 288 "cc.y"
        {
		(yyval.node) = new(OIND, (Z), Z);
		(yyval.node)->garb = simpleg((yyvsp[0].lval));
	}
#line 2156 "y.tab.c"
    break;

  case 43: /* abdecor1: '*' zgnlist abdecor1  */
#line 293 "cc.y"
        {
		(yyval.node) = new(OIND, (yyvsp[0].node), Z);
		(yyval.node)->garb = simpleg((yyvsp[-1].lval));
	}
#line 2165 "y.tab.c"
    break;

  case 46: /* abdecor2: abdecor2 '(' zarglist ')'  */
#line 302 "cc.y"
        {
		(yyval.node) = new(OFUNC, (yyvsp[-3].node), (yyvsp[-1].node));
	}
#line 2173 "y.tab.c"
    break;

  case 47: /* abdecor2: abdecor2 '[' zexpr ']'  */
#line 306 "cc.y"
        {
		(yyval.node) = new(OARRAY, (yyvsp[-3].node), (yyvsp[-1].node));
	}
#line 2181 "y.tab.c"
    break;

  case 48: /* abdecor3: '(' ')'  */
#line 312 "cc.y"
        {
		(yyval.node) = new(OFUNC, (Z), Z);
	}
#line 2189 "y.tab.c"
    break;

  case 49: /* abdecor3: '[' zexpr ']'  */
#line 316 "cc.y"
        {
		(yyval.node) = new(OARRAY, (Z), (yyvsp[-1].node));
	}
#line 2197 "y.tab.c"
    break;

  case 50: /* abdecor3: '(' abdecor1 ')'  */
#line 320 "cc.y"
        {
		(yyval.node) = (yyvsp[-1].node);
	}
#line 2205 "y.tab.c"
    break;

  case 52: /* init: '{' ilist '}'  */
#line 327 "cc.y"
        {
		(yyval.node) = new(OINIT, invert((yyvsp[-1].node)), Z);
	}
#line 2213 "y.tab.c"
    break;

  case 53: /* qual: '[' lexpr ']'  */
#line 333 "cc.y"
        {
		(yyval.node) = new(OARRAY, (yyvsp[-1].node), Z);
	}
#line 2221 "y.tab.c"
    break;

  case 54: /* qual: '.' ltag  */
#line 337 "cc.y"
        {
		(yyval.node) = new(OELEM, Z, Z);
		(yyval.node)->sym = (yyvsp[0].sym);
	}
#line 2230 "y.tab.c"
    break;

  case 57: /* qlist: qlist init ','  */
#line 346 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-2].node), (yyvsp[-1].node));
	}
#line 2238 "y.tab.c"
    break;

  case 59: /* qlist: qlist qual  */
#line 351 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-1].node), (yyvsp[0].node));
	}
#line 2246 "y.tab.c"
    break;

  case 62: /* ilist: qlist init  */
#line 359 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-1].node), (yyvsp[0].node));
	}
#line 2254 "y.tab.c"
    break;

  case 63: /* zarglist: %empty  */
#line 364 "cc.y"
        {
		(yyval.node) = Z;
	}
#line 2262 "y.tab.c"
    break;

  case 64: /* zarglist: arglist  */
#line 368 "cc.y"
        {
		(yyval.node) = invert((yyvsp[0].node));
	}
#line 2270 "y.tab.c"
    break;

  case 66: /* arglist: tlist abdecor  */
#line 376 "cc.y"
        {
		(yyval.node) = new(OPROTO, (yyvsp[0].node), Z);
		(yyval.node)->type = (yyvsp[-1].type);
	}
#line 2279 "y.tab.c"
    break;

  case 67: /* arglist: tlist xdecor  */
#line 381 "cc.y"
        {
		(yyval.node) = new(OPROTO, (yyvsp[0].node), Z);
		(yyval.node)->type = (yyvsp[-1].type);
	}
#line 2288 "y.tab.c"
    break;

  case 68: /* arglist: '.' '.' '.'  */
#line 386 "cc.y"
        {
		(yyval.node) = new(ODOTDOT, Z, Z);
	}
#line 2296 "y.tab.c"
    break;

  case 69: /* arglist: arglist ',' arglist  */
#line 390 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2304 "y.tab.c"
    break;

  case 70: /* block: '{' slist '}'  */
#line 396 "cc.y"
        {
		(yyval.node) = invert((yyvsp[-1].node));
	//	if($2 != Z)
	//		$$ = new(OLIST, $2, $$);
		if((yyval.node) == Z)
			(yyval.node) = new(OLIST, Z, Z);
	}
#line 2316 "y.tab.c"
    break;

  case 71: /* slist: %empty  */
#line 405 "cc.y"
        {
		(yyval.node) = Z;
	}
#line 2324 "y.tab.c"
    break;

  case 72: /* slist: slist adecl  */
#line 409 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-1].node), (yyvsp[0].node));
	}
#line 2332 "y.tab.c"
    break;

  case 73: /* slist: slist stmnt  */
#line 413 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-1].node), (yyvsp[0].node));
	}
#line 2340 "y.tab.c"
    break;

  case 75: /* labels: labels label  */
#line 420 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-1].node), (yyvsp[0].node));
	}
#line 2348 "y.tab.c"
    break;

  case 76: /* label: LCASE expr ':'  */
#line 426 "cc.y"
        {
		(yyval.node) = new(OCASE, (yyvsp[-1].node), Z);
	}
#line 2356 "y.tab.c"
    break;

  case 77: /* label: LDEFAULT ':'  */
#line 430 "cc.y"
        {
		(yyval.node) = new(OCASE, Z, Z);
	}
#line 2364 "y.tab.c"
    break;

  case 78: /* label: LNAME ':'  */
#line 434 "cc.y"
        {
		(yyval.node) = new(OLABEL, dcllabel((yyvsp[-1].sym), 1), Z);
	}
#line 2372 "y.tab.c"
    break;

  case 79: /* stmnt: error ';'  */
#line 440 "cc.y"
        {
		(yyval.node) = Z;
	}
#line 2380 "y.tab.c"
    break;

  case 81: /* stmnt: labels ulstmnt  */
#line 445 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-1].node), (yyvsp[0].node));
	}
#line 2388 "y.tab.c"
    break;

  case 83: /* forexpr: ctlist adlist  */
#line 452 "cc.y"
        {
		(yyval.node) = (yyvsp[0].node);
	}
#line 2396 "y.tab.c"
    break;

  case 85: /* $@7: %empty  */
#line 458 "cc.y"
        {
		markdcl();
	}
#line 2404 "y.tab.c"
    break;

  case 86: /* ulstmnt: $@7 block  */
#line 462 "cc.y"
        {
		(yyval.node) = revertdcl();
		if((yyval.node))
			(yyval.node) = new(OLIST, (yyval.node), (yyvsp[0].node));
		else
			(yyval.node) = (yyvsp[0].node);
	}
#line 2416 "y.tab.c"
    break;

  case 87: /* ulstmnt: LIF '(' cexpr ')' stmnt  */
#line 470 "cc.y"
        {
		(yyval.node) = new(OIF, (yyvsp[-2].node), new(OLIST, (yyvsp[0].node), Z));
		if((yyvsp[0].node) == Z)
			warn((yyvsp[-2].node), "empty if body");
	}
#line 2426 "y.tab.c"
    break;

  case 88: /* ulstmnt: LIF '(' cexpr ')' stmnt LELSE stmnt  */
#line 476 "cc.y"
        {
		(yyval.node) = new(OIF, (yyvsp[-4].node), new(OLIST, (yyvsp[-2].node), (yyvsp[0].node)));
		if((yyvsp[-2].node) == Z)
			warn((yyvsp[-4].node), "empty if body");
		if((yyvsp[0].node) == Z)
			warn((yyvsp[-4].node), "empty else body");
	}
#line 2438 "y.tab.c"
    break;

  case 89: /* $@8: %empty  */
#line 483 "cc.y"
        { markdcl(); }
#line 2444 "y.tab.c"
    break;

  case 90: /* ulstmnt: $@8 LFOR '(' forexpr ';' zcexpr ';' zcexpr ')' stmnt  */
#line 484 "cc.y"
        {
		(yyval.node) = revertdcl();
		if((yyval.node)){
			if((yyvsp[-6].node))
				(yyvsp[-6].node) = new(OLIST, (yyval.node), (yyvsp[-6].node));
			else
				(yyvsp[-6].node) = (yyval.node);
		}
		(yyval.node) = new(OFOR, new(OLIST, (yyvsp[-4].node), new(OLIST, (yyvsp[-6].node), (yyvsp[-2].node))), (yyvsp[0].node));
	}
#line 2459 "y.tab.c"
    break;

  case 91: /* ulstmnt: LWHILE '(' cexpr ')' stmnt  */
#line 495 "cc.y"
        {
		(yyval.node) = new(OWHILE, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2467 "y.tab.c"
    break;

  case 92: /* ulstmnt: LDO stmnt LWHILE '(' cexpr ')' ';'  */
#line 499 "cc.y"
        {
		(yyval.node) = new(ODWHILE, (yyvsp[-2].node), (yyvsp[-5].node));
	}
#line 2475 "y.tab.c"
    break;

  case 93: /* ulstmnt: LRETURN zcexpr ';'  */
#line 503 "cc.y"
        {
		(yyval.node) = new(ORETURN, (yyvsp[-1].node), Z);
		(yyval.node)->type = thisfn->link;
	}
#line 2484 "y.tab.c"
    break;

  case 94: /* ulstmnt: LSWITCH '(' cexpr ')' stmnt  */
#line 508 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->vconst = 0;
		(yyval.node)->type = types[TINT];
		(yyvsp[-2].node) = new(OSUB, (yyval.node), (yyvsp[-2].node));

		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->vconst = 0;
		(yyval.node)->type = types[TINT];
		(yyvsp[-2].node) = new(OSUB, (yyval.node), (yyvsp[-2].node));

		(yyval.node) = new(OSWITCH, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2502 "y.tab.c"
    break;

  case 95: /* ulstmnt: LBREAK ';'  */
#line 522 "cc.y"
        {
		(yyval.node) = new(OBREAK, Z, Z);
	}
#line 2510 "y.tab.c"
    break;

  case 96: /* ulstmnt: LCONTINUE ';'  */
#line 526 "cc.y"
        {
		(yyval.node) = new(OCONTINUE, Z, Z);
	}
#line 2518 "y.tab.c"
    break;

  case 97: /* ulstmnt: LGOTO ltag ';'  */
#line 530 "cc.y"
        {
		(yyval.node) = new(OGOTO, dcllabel((yyvsp[-1].sym), 0), Z);
	}
#line 2526 "y.tab.c"
    break;

  case 98: /* ulstmnt: LUSED '(' zelist ')' ';'  */
#line 534 "cc.y"
        {
		(yyval.node) = new(OUSED, (yyvsp[-2].node), Z);
	}
#line 2534 "y.tab.c"
    break;

  case 99: /* ulstmnt: LSET '(' zelist ')' ';'  */
#line 538 "cc.y"
        {
		(yyval.node) = new(OSET, (yyvsp[-2].node), Z);
	}
#line 2542 "y.tab.c"
    break;

  case 100: /* zcexpr: %empty  */
#line 543 "cc.y"
        {
		(yyval.node) = Z;
	}
#line 2550 "y.tab.c"
    break;

  case 102: /* zexpr: %empty  */
#line 549 "cc.y"
        {
		(yyval.node) = Z;
	}
#line 2558 "y.tab.c"
    break;

  case 104: /* lexpr: expr  */
#line 556 "cc.y"
        {
		(yyval.node) = new(OCAST, (yyvsp[0].node), Z);
		(yyval.node)->type = types[TLONG];
	}
#line 2567 "y.tab.c"
    break;

  case 106: /* cexpr: cexpr ',' cexpr  */
#line 564 "cc.y"
        {
		(yyval.node) = new(OCOMMA, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2575 "y.tab.c"
    break;

  case 108: /* expr: expr '*' expr  */
#line 571 "cc.y"
        {
		(yyval.node) = new(OMUL, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2583 "y.tab.c"
    break;

  case 109: /* expr: expr '/' expr  */
#line 575 "cc.y"
        {
		(yyval.node) = new(ODIV, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2591 "y.tab.c"
    break;

  case 110: /* expr: expr '%' expr  */
#line 579 "cc.y"
        {
		(yyval.node) = new(OMOD, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2599 "y.tab.c"
    break;

  case 111: /* expr: expr '+' expr  */
#line 583 "cc.y"
        {
		(yyval.node) = new(OADD, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2607 "y.tab.c"
    break;

  case 112: /* expr: expr '-' expr  */
#line 587 "cc.y"
        {
		(yyval.node) = new(OSUB, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2615 "y.tab.c"
    break;

  case 113: /* expr: expr LRSH expr  */
#line 591 "cc.y"
        {
		(yyval.node) = new(OASHR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2623 "y.tab.c"
    break;

  case 114: /* expr: expr LLSH expr  */
#line 595 "cc.y"
        {
		(yyval.node) = new(OASHL, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2631 "y.tab.c"
    break;

  case 115: /* expr: expr '<' expr  */
#line 599 "cc.y"
        {
		(yyval.node) = new(OLT, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2639 "y.tab.c"
    break;

  case 116: /* expr: expr '>' expr  */
#line 603 "cc.y"
        {
		(yyval.node) = new(OGT, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2647 "y.tab.c"
    break;

  case 117: /* expr: expr LLE expr  */
#line 607 "cc.y"
        {
		(yyval.node) = new(OLE, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2655 "y.tab.c"
    break;

  case 118: /* expr: expr LGE expr  */
#line 611 "cc.y"
        {
		(yyval.node) = new(OGE, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2663 "y.tab.c"
    break;

  case 119: /* expr: expr LEQ expr  */
#line 615 "cc.y"
        {
		(yyval.node) = new(OEQ, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2671 "y.tab.c"
    break;

  case 120: /* expr: expr LNE expr  */
#line 619 "cc.y"
        {
		(yyval.node) = new(ONE, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2679 "y.tab.c"
    break;

  case 121: /* expr: expr '&' expr  */
#line 623 "cc.y"
        {
		(yyval.node) = new(OAND, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2687 "y.tab.c"
    break;

  case 122: /* expr: expr '^' expr  */
#line 627 "cc.y"
        {
		(yyval.node) = new(OXOR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2695 "y.tab.c"
    break;

  case 123: /* expr: expr '|' expr  */
#line 631 "cc.y"
        {
		(yyval.node) = new(OOR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2703 "y.tab.c"
    break;

  case 124: /* expr: expr LANDAND expr  */
#line 635 "cc.y"
        {
		(yyval.node) = new(OANDAND, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2711 "y.tab.c"
    break;

  case 125: /* expr: expr LOROR expr  */
#line 639 "cc.y"
        {
		(yyval.node) = new(OOROR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2719 "y.tab.c"
    break;

  case 126: /* expr: expr '?' cexpr ':' expr  */
#line 643 "cc.y"
        {
		(yyval.node) = new(OCOND, (yyvsp[-4].node), new(OLIST, (yyvsp[-2].node), (yyvsp[0].node)));
	}
#line 2727 "y.tab.c"
    break;

  case 127: /* expr: expr '=' expr  */
#line 647 "cc.y"
        {
		(yyval.node) = new(OAS, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2735 "y.tab.c"
    break;

  case 128: /* expr: expr LPE expr  */
#line 651 "cc.y"
        {
		(yyval.node) = new(OASADD, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2743 "y.tab.c"
    break;

  case 129: /* expr: expr LME expr  */
#line 655 "cc.y"
        {
		(yyval.node) = new(OASSUB, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2751 "y.tab.c"
    break;

  case 130: /* expr: expr LMLE expr  */
#line 659 "cc.y"
        {
		(yyval.node) = new(OASMUL, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2759 "y.tab.c"
    break;

  case 131: /* expr: expr LDVE expr  */
#line 663 "cc.y"
        {
		(yyval.node) = new(OASDIV, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2767 "y.tab.c"
    break;

  case 132: /* expr: expr LMDE expr  */
#line 667 "cc.y"
        {
		(yyval.node) = new(OASMOD, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2775 "y.tab.c"
    break;

  case 133: /* expr: expr LLSHE expr  */
#line 671 "cc.y"
        {
		(yyval.node) = new(OASASHL, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2783 "y.tab.c"
    break;

  case 134: /* expr: expr LRSHE expr  */
#line 675 "cc.y"
        {
		(yyval.node) = new(OASASHR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2791 "y.tab.c"
    break;

  case 135: /* expr: expr LANDE expr  */
#line 679 "cc.y"
        {
		(yyval.node) = new(OASAND, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2799 "y.tab.c"
    break;

  case 136: /* expr: expr LXORE expr  */
#line 683 "cc.y"
        {
		(yyval.node) = new(OASXOR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2807 "y.tab.c"
    break;

  case 137: /* expr: expr LORE expr  */
#line 687 "cc.y"
        {
		(yyval.node) = new(OASOR, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 2815 "y.tab.c"
    break;

  case 139: /* xuexpr: '(' tlist abdecor ')' xuexpr  */
#line 694 "cc.y"
        {
		(yyval.node) = new(OCAST, (yyvsp[0].node), Z);
		dodecl(NODECL, CXXX, (yyvsp[-3].type), (yyvsp[-2].node));
		(yyval.node)->type = lastdcl;
		(yyval.node)->xcast = 1;
	}
#line 2826 "y.tab.c"
    break;

  case 140: /* xuexpr: '(' tlist abdecor ')' '{' ilist '}'  */
#line 701 "cc.y"
        {
		(yyval.node) = new(OSTRUCT, (yyvsp[-1].node), Z);
		dodecl(NODECL, CXXX, (yyvsp[-5].type), (yyvsp[-4].node));
		(yyval.node)->type = lastdcl;
	}
#line 2836 "y.tab.c"
    break;

  case 142: /* uexpr: '*' xuexpr  */
#line 710 "cc.y"
        {
		(yyval.node) = new(OIND, (yyvsp[0].node), Z);
	}
#line 2844 "y.tab.c"
    break;

  case 143: /* uexpr: '&' xuexpr  */
#line 714 "cc.y"
        {
		(yyval.node) = new(OADDR, (yyvsp[0].node), Z);
	}
#line 2852 "y.tab.c"
    break;

  case 144: /* uexpr: '+' xuexpr  */
#line 718 "cc.y"
        {
		(yyval.node) = new(OPOS, (yyvsp[0].node), Z);
	}
#line 2860 "y.tab.c"
    break;

  case 145: /* uexpr: '-' xuexpr  */
#line 722 "cc.y"
        {
		(yyval.node) = new(ONEG, (yyvsp[0].node), Z);
	}
#line 2868 "y.tab.c"
    break;

  case 146: /* uexpr: '!' xuexpr  */
#line 726 "cc.y"
        {
		(yyval.node) = new(ONOT, (yyvsp[0].node), Z);
	}
#line 2876 "y.tab.c"
    break;

  case 147: /* uexpr: '~' xuexpr  */
#line 730 "cc.y"
        {
		(yyval.node) = new(OCOM, (yyvsp[0].node), Z);
	}
#line 2884 "y.tab.c"
    break;

  case 148: /* uexpr: LPP xuexpr  */
#line 734 "cc.y"
        {
		(yyval.node) = new(OPREINC, (yyvsp[0].node), Z);
	}
#line 2892 "y.tab.c"
    break;

  case 149: /* uexpr: LMM xuexpr  */
#line 738 "cc.y"
        {
		(yyval.node) = new(OPREDEC, (yyvsp[0].node), Z);
	}
#line 2900 "y.tab.c"
    break;

  case 150: /* uexpr: LSIZEOF uexpr  */
#line 742 "cc.y"
        {
		(yyval.node) = new(OSIZE, (yyvsp[0].node), Z);
	}
#line 2908 "y.tab.c"
    break;

  case 151: /* uexpr: LSIGNOF uexpr  */
#line 746 "cc.y"
        {
		(yyval.node) = new(OSIGN, (yyvsp[0].node), Z);
	}
#line 2916 "y.tab.c"
    break;

  case 152: /* pexpr: '(' cexpr ')'  */
#line 752 "cc.y"
        {
		(yyval.node) = (yyvsp[-1].node);
	}
#line 2924 "y.tab.c"
    break;

  case 153: /* pexpr: LSIZEOF '(' tlist abdecor ')'  */
#line 756 "cc.y"
        {
		(yyval.node) = new(OSIZE, Z, Z);
		dodecl(NODECL, CXXX, (yyvsp[-2].type), (yyvsp[-1].node));
		(yyval.node)->type = lastdcl;
	}
#line 2934 "y.tab.c"
    break;

  case 154: /* pexpr: LSIGNOF '(' tlist abdecor ')'  */
#line 762 "cc.y"
        {
		(yyval.node) = new(OSIGN, Z, Z);
		dodecl(NODECL, CXXX, (yyvsp[-2].type), (yyvsp[-1].node));
		(yyval.node)->type = lastdcl;
	}
#line 2944 "y.tab.c"
    break;

  case 155: /* pexpr: pexpr '(' zelist ')'  */
#line 768 "cc.y"
        {
		(yyval.node) = new(OFUNC, (yyvsp[-3].node), Z);
		if((yyvsp[-3].node)->op == ONAME)
		if((yyvsp[-3].node)->type == T)
			dodecl(xdecl, CXXX, types[TINT], (yyval.node));
		(yyval.node)->right = invert((yyvsp[-1].node));
	}
#line 2956 "y.tab.c"
    break;

  case 156: /* pexpr: pexpr '[' cexpr ']'  */
#line 776 "cc.y"
        {
		(yyval.node) = new(OIND, new(OADD, (yyvsp[-3].node), (yyvsp[-1].node)), Z);
	}
#line 2964 "y.tab.c"
    break;

  case 157: /* pexpr: pexpr LMG ltag  */
#line 780 "cc.y"
        {
		(yyval.node) = new(ODOT, new(OIND, (yyvsp[-2].node), Z), Z);
		(yyval.node)->sym = (yyvsp[0].sym);
	}
#line 2973 "y.tab.c"
    break;

  case 158: /* pexpr: pexpr '.' ltag  */
#line 785 "cc.y"
        {
		(yyval.node) = new(ODOT, (yyvsp[-2].node), Z);
		(yyval.node)->sym = (yyvsp[0].sym);
	}
#line 2982 "y.tab.c"
    break;

  case 159: /* pexpr: pexpr LPP  */
#line 790 "cc.y"
        {
		(yyval.node) = new(OPOSTINC, (yyvsp[-1].node), Z);
	}
#line 2990 "y.tab.c"
    break;

  case 160: /* pexpr: pexpr LMM  */
#line 794 "cc.y"
        {
		(yyval.node) = new(OPOSTDEC, (yyvsp[-1].node), Z);
	}
#line 2998 "y.tab.c"
    break;

  case 162: /* pexpr: LCONST  */
#line 799 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TINT];
		(yyval.node)->vconst = (yyvsp[0].vval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3009 "y.tab.c"
    break;

  case 163: /* pexpr: LLCONST  */
#line 806 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TLONG];
		(yyval.node)->vconst = (yyvsp[0].vval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3020 "y.tab.c"
    break;

  case 164: /* pexpr: LUCONST  */
#line 813 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TUINT];
		(yyval.node)->vconst = (yyvsp[0].vval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3031 "y.tab.c"
    break;

  case 165: /* pexpr: LULCONST  */
#line 820 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TULONG];
		(yyval.node)->vconst = (yyvsp[0].vval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3042 "y.tab.c"
    break;

  case 166: /* pexpr: LDCONST  */
#line 827 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TDOUBLE];
		(yyval.node)->fconst = (yyvsp[0].dval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3053 "y.tab.c"
    break;

  case 167: /* pexpr: LFCONST  */
#line 834 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TFLOAT];
		(yyval.node)->fconst = (yyvsp[0].dval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3064 "y.tab.c"
    break;

  case 168: /* pexpr: LVLCONST  */
#line 841 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TVLONG];
		(yyval.node)->vconst = (yyvsp[0].vval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3075 "y.tab.c"
    break;

  case 169: /* pexpr: LUVLCONST  */
#line 848 "cc.y"
        {
		(yyval.node) = new(OCONST, Z, Z);
		(yyval.node)->type = types[TUVLONG];
		(yyval.node)->vconst = (yyvsp[0].vval);
		(yyval.node)->cstring = strdup(symb);
	}
#line 3086 "y.tab.c"
    break;

  case 172: /* string: LSTRING  */
#line 859 "cc.y"
        {
		(yyval.node) = new(OSTRING, Z, Z);
		(yyval.node)->type = typ(TARRAY, types[TCHAR]);
		(yyval.node)->type->width = (yyvsp[0].sval).l + 1;
		(yyval.node)->cstring = (yyvsp[0].sval).s;
		(yyval.node)->sym = symstring;
		(yyval.node)->etype = TARRAY;
		(yyval.node)->class = CSTATIC;
	}
#line 3100 "y.tab.c"
    break;

  case 173: /* string: string LSTRING  */
#line 869 "cc.y"
        {
		char *s;
		int n;

		n = (yyvsp[-1].node)->type->width - 1;
		s = alloc(n+(yyvsp[0].sval).l+MAXALIGN);

		memcpy(s, (yyvsp[-1].node)->cstring, n);
		memcpy(s+n, (yyvsp[0].sval).s, (yyvsp[0].sval).l);
		s[n+(yyvsp[0].sval).l] = 0;

		(yyval.node) = (yyvsp[-1].node);
		(yyval.node)->type->width += (yyvsp[0].sval).l;
		(yyval.node)->cstring = s;
	}
#line 3120 "y.tab.c"
    break;

  case 174: /* lstring: LLSTRING  */
#line 887 "cc.y"
        {
		(yyval.node) = new(OLSTRING, Z, Z);
		(yyval.node)->type = typ(TARRAY, types[TUSHORT]);
		(yyval.node)->type->width = (yyvsp[0].sval).l + sizeof(ushort);
		(yyval.node)->rstring = (ushort*)(yyvsp[0].sval).s;
		(yyval.node)->sym = symstring;
		(yyval.node)->etype = TARRAY;
		(yyval.node)->class = CSTATIC;
	}
#line 3134 "y.tab.c"
    break;

  case 175: /* lstring: lstring LLSTRING  */
#line 897 "cc.y"
        {
		char *s;
		int n;

		n = (yyvsp[-1].node)->type->width - sizeof(ushort);
		s = alloc(n+(yyvsp[0].sval).l+MAXALIGN);

		memcpy(s, (yyvsp[-1].node)->rstring, n);
		memcpy(s+n, (yyvsp[0].sval).s, (yyvsp[0].sval).l);
		*(ushort*)(s+n+(yyvsp[0].sval).l) = 0;

		(yyval.node) = (yyvsp[-1].node);
		(yyval.node)->type->width += (yyvsp[0].sval).l;
		(yyval.node)->rstring = (ushort*)s;
	}
#line 3154 "y.tab.c"
    break;

  case 176: /* zelist: %empty  */
#line 914 "cc.y"
        {
		(yyval.node) = Z;
	}
#line 3162 "y.tab.c"
    break;

  case 179: /* elist: elist ',' elist  */
#line 922 "cc.y"
        {
		(yyval.node) = new(OLIST, (yyvsp[-2].node), (yyvsp[0].node));
	}
#line 3170 "y.tab.c"
    break;

  case 180: /* @9: %empty  */
#line 928 "cc.y"
        {
		(yyval.tyty).t1 = strf;
		(yyval.tyty).t2 = strl;
		(yyval.tyty).t3 = lasttype;
		(yyval.tyty).c = lastclass;
		strf = T;
		strl = T;
		lastbit = 0;
		firstbit = 1;
		lastclass = CXXX;
		lasttype = T;
	}
#line 3187 "y.tab.c"
    break;

  case 181: /* sbody: '{' @9 edecl '}'  */
#line 941 "cc.y"
        {
		(yyval.type) = strf;
		strf = (yyvsp[-2].tyty).t1;
		strl = (yyvsp[-2].tyty).t2;
		lasttype = (yyvsp[-2].tyty).t3;
		lastclass = (yyvsp[-2].tyty).c;
	}
#line 3199 "y.tab.c"
    break;

  case 182: /* zctlist: %empty  */
#line 950 "cc.y"
        {
		lastclass = CXXX;
		lasttype = types[TINT];
	}
#line 3208 "y.tab.c"
    break;

  case 184: /* types: complex  */
#line 958 "cc.y"
        {
		(yyval.tycl).t = (yyvsp[0].type);
		(yyval.tycl).c = CXXX;
	}
#line 3217 "y.tab.c"
    break;

  case 185: /* types: tname  */
#line 963 "cc.y"
        {
		(yyval.tycl).t = simplet((yyvsp[0].lval));
		(yyval.tycl).c = CXXX;
	}
#line 3226 "y.tab.c"
    break;

  case 186: /* types: gcnlist  */
#line 968 "cc.y"
        {
		(yyval.tycl).t = simplet((yyvsp[0].lval));
		(yyval.tycl).c = simplec((yyvsp[0].lval));
		(yyval.tycl).t = garbt((yyval.tycl).t, (yyvsp[0].lval));
	}
#line 3236 "y.tab.c"
    break;

  case 187: /* types: complex gctnlist  */
#line 974 "cc.y"
        {
		(yyval.tycl).t = (yyvsp[-1].type);
		(yyval.tycl).c = simplec((yyvsp[0].lval));
		(yyval.tycl).t = garbt((yyval.tycl).t, (yyvsp[0].lval));
		if((yyvsp[0].lval) & ~BCLASS & ~BGARB)
			diag(Z, "duplicate types given: %T and %Q", (yyvsp[-1].type), (yyvsp[0].lval));
	}
#line 3248 "y.tab.c"
    break;

  case 188: /* types: tname gctnlist  */
#line 982 "cc.y"
        {
		(yyval.tycl).t = simplet(typebitor((yyvsp[-1].lval), (yyvsp[0].lval)));
		(yyval.tycl).c = simplec((yyvsp[0].lval));
		(yyval.tycl).t = garbt((yyval.tycl).t, (yyvsp[0].lval));
	}
#line 3258 "y.tab.c"
    break;

  case 189: /* types: gcnlist complex zgnlist  */
#line 988 "cc.y"
        {
		(yyval.tycl).t = (yyvsp[-1].type);
		(yyval.tycl).c = simplec((yyvsp[-2].lval));
		(yyval.tycl).t = garbt((yyval.tycl).t, (yyvsp[-2].lval)|(yyvsp[0].lval));
	}
#line 3268 "y.tab.c"
    break;

  case 190: /* types: gcnlist tname  */
#line 994 "cc.y"
        {
		(yyval.tycl).t = simplet((yyvsp[0].lval));
		(yyval.tycl).c = simplec((yyvsp[-1].lval));
		(yyval.tycl).t = garbt((yyval.tycl).t, (yyvsp[-1].lval));
	}
#line 3278 "y.tab.c"
    break;

  case 191: /* types: gcnlist tname gctnlist  */
#line 1000 "cc.y"
        {
		(yyval.tycl).t = simplet(typebitor((yyvsp[-1].lval), (yyvsp[0].lval)));
		(yyval.tycl).c = simplec((yyvsp[-2].lval)|(yyvsp[0].lval));
		(yyval.tycl).t = garbt((yyval.tycl).t, (yyvsp[-2].lval)|(yyvsp[0].lval));
	}
#line 3288 "y.tab.c"
    break;

  case 192: /* tlist: types  */
#line 1008 "cc.y"
        {
		(yyval.type) = (yyvsp[0].tycl).t;
		if((yyvsp[0].tycl).c != CXXX)
			diag(Z, "illegal combination of class 4: %s", cnames[(yyvsp[0].tycl).c]);
	}
#line 3298 "y.tab.c"
    break;

  case 193: /* ctlist: types  */
#line 1016 "cc.y"
        {
		lasttype = (yyvsp[0].tycl).t;
		lastclass = (yyvsp[0].tycl).c;
	}
#line 3307 "y.tab.c"
    break;

  case 194: /* complex: LSTRUCT ltag  */
#line 1023 "cc.y"
        {
		dotag((yyvsp[0].sym), TSTRUCT, 0);
		(yyval.type) = (yyvsp[0].sym)->suetag;
	}
#line 3316 "y.tab.c"
    break;

  case 195: /* $@10: %empty  */
#line 1028 "cc.y"
        {
		dotag((yyvsp[0].sym), TSTRUCT, autobn);
	}
#line 3324 "y.tab.c"
    break;

  case 196: /* complex: LSTRUCT ltag $@10 sbody  */
#line 1032 "cc.y"
        {
		(yyval.type) = (yyvsp[-2].sym)->suetag;
		if((yyval.type)->link != T)
			diag(Z, "redeclare tag: %s", (yyvsp[-2].sym)->name);
		(yyval.type)->link = (yyvsp[0].type);
		sualign((yyval.type));
	}
#line 3336 "y.tab.c"
    break;

  case 197: /* complex: LSTRUCT sbody  */
#line 1040 "cc.y"
        {
		taggen++;
		sprint(symb, "_%d_", taggen);
		(yyval.type) = dotag(lookup(), TSTRUCT, autobn);
		(yyval.type)->link = (yyvsp[0].type);
		sualign((yyval.type));
	}
#line 3348 "y.tab.c"
    break;

  case 198: /* complex: LUNION ltag  */
#line 1048 "cc.y"
        {
		dotag((yyvsp[0].sym), TUNION, 0);
		(yyval.type) = (yyvsp[0].sym)->suetag;
	}
#line 3357 "y.tab.c"
    break;

  case 199: /* $@11: %empty  */
#line 1053 "cc.y"
        {
		dotag((yyvsp[0].sym), TUNION, autobn);
	}
#line 3365 "y.tab.c"
    break;

  case 200: /* complex: LUNION ltag $@11 sbody  */
#line 1057 "cc.y"
        {
		(yyval.type) = (yyvsp[-2].sym)->suetag;
		if((yyval.type)->link != T)
			diag(Z, "redeclare tag: %s", (yyvsp[-2].sym)->name);
		(yyval.type)->link = (yyvsp[0].type);
		sualign((yyval.type));
	}
#line 3377 "y.tab.c"
    break;

  case 201: /* complex: LUNION sbody  */
#line 1065 "cc.y"
        {
		taggen++;
		sprint(symb, "_%d_", taggen);
		(yyval.type) = dotag(lookup(), TUNION, autobn);
		(yyval.type)->link = (yyvsp[0].type);
		sualign((yyval.type));
	}
#line 3389 "y.tab.c"
    break;

  case 202: /* complex: LENUM ltag  */
#line 1073 "cc.y"
        {
		dotag((yyvsp[0].sym), TENUM, 0);
		(yyval.type) = (yyvsp[0].sym)->suetag;
		if((yyval.type)->link == T)
			(yyval.type)->link = types[TINT];
		(yyval.type) = (yyval.type)->link;
	}
#line 3401 "y.tab.c"
    break;

  case 203: /* $@12: %empty  */
#line 1081 "cc.y"
        {
		dotag((yyvsp[0].sym), TENUM, autobn);
	}
#line 3409 "y.tab.c"
    break;

  case 204: /* $@13: %empty  */
#line 1085 "cc.y"
        {
		en.tenum = T;
		en.cenum = T;
	}
#line 3418 "y.tab.c"
    break;

  case 205: /* complex: LENUM ltag $@12 '{' $@13 enum '}'  */
#line 1090 "cc.y"
        {
		(yyval.type) = (yyvsp[-5].sym)->suetag;
		if((yyval.type)->link != T)
			diag(Z, "redeclare tag: %s", (yyvsp[-5].sym)->name);
		if(en.tenum == T) {
			diag(Z, "enum type ambiguous: %s", (yyvsp[-5].sym)->name);
			en.tenum = types[TINT];
		}
		(yyval.type)->link = en.tenum;
		(yyval.type) = en.tenum;
	}
#line 3434 "y.tab.c"
    break;

  case 206: /* $@14: %empty  */
#line 1102 "cc.y"
        {
		en.tenum = T;
		en.cenum = T;
	}
#line 3443 "y.tab.c"
    break;

  case 207: /* complex: LENUM '{' $@14 enum '}'  */
#line 1107 "cc.y"
        {
		(yyval.type) = en.tenum;
	}
#line 3451 "y.tab.c"
    break;

  case 208: /* complex: LTYPE  */
#line 1111 "cc.y"
        {
		(yyval.type) = tcopy((yyvsp[0].sym)->type);
	}
#line 3459 "y.tab.c"
    break;

  case 210: /* gctnlist: gctnlist gctname  */
#line 1118 "cc.y"
        {
		(yyval.lval) = typebitor((yyvsp[-1].lval), (yyvsp[0].lval));
	}
#line 3467 "y.tab.c"
    break;

  case 211: /* zgnlist: %empty  */
#line 1123 "cc.y"
        {
		(yyval.lval) = 0;
	}
#line 3475 "y.tab.c"
    break;

  case 212: /* zgnlist: zgnlist gname  */
#line 1127 "cc.y"
        {
		(yyval.lval) = typebitor((yyvsp[-1].lval), (yyvsp[0].lval));
	}
#line 3483 "y.tab.c"
    break;

  case 217: /* gcnlist: gcnlist gcname  */
#line 1139 "cc.y"
        {
		(yyval.lval) = typebitor((yyvsp[-1].lval), (yyvsp[0].lval));
	}
#line 3491 "y.tab.c"
    break;

  case 220: /* enum: LNAME  */
#line 1149 "cc.y"
        {
		doenum((yyvsp[0].sym), Z);
	}
#line 3499 "y.tab.c"
    break;

  case 221: /* enum: LNAME '=' expr  */
#line 1153 "cc.y"
        {
		doenum((yyvsp[-2].sym), (yyvsp[0].node));
	}
#line 3507 "y.tab.c"
    break;

  case 224: /* tname: LCHAR  */
#line 1160 "cc.y"
              { (yyval.lval) = BCHAR; }
#line 3513 "y.tab.c"
    break;

  case 225: /* tname: LSHORT  */
#line 1161 "cc.y"
               { (yyval.lval) = BSHORT; }
#line 3519 "y.tab.c"
    break;

  case 226: /* tname: LINT  */
#line 1162 "cc.y"
             { (yyval.lval) = BINT; }
#line 3525 "y.tab.c"
    break;

  case 227: /* tname: LLONG  */
#line 1163 "cc.y"
              { (yyval.lval) = BLONG; }
#line 3531 "y.tab.c"
    break;

  case 228: /* tname: LSIGNED  */
#line 1164 "cc.y"
                { (yyval.lval) = BSIGNED; }
#line 3537 "y.tab.c"
    break;

  case 229: /* tname: LUNSIGNED  */
#line 1165 "cc.y"
                  { (yyval.lval) = BUNSIGNED; }
#line 3543 "y.tab.c"
    break;

  case 230: /* tname: LFLOAT  */
#line 1166 "cc.y"
               { (yyval.lval) = BFLOAT; }
#line 3549 "y.tab.c"
    break;

  case 231: /* tname: LDOUBLE  */
#line 1167 "cc.y"
                { (yyval.lval) = BDOUBLE; }
#line 3555 "y.tab.c"
    break;

  case 232: /* tname: LVOID  */
#line 1168 "cc.y"
              { (yyval.lval) = BVOID; }
#line 3561 "y.tab.c"
    break;

  case 233: /* cname: LAUTO  */
#line 1171 "cc.y"
              { (yyval.lval) = BAUTO; }
#line 3567 "y.tab.c"
    break;

  case 234: /* cname: LSTATIC  */
#line 1172 "cc.y"
                { (yyval.lval) = BSTATIC; }
#line 3573 "y.tab.c"
    break;

  case 235: /* cname: LEXTERN  */
#line 1173 "cc.y"
                { (yyval.lval) = BEXTERN; }
#line 3579 "y.tab.c"
    break;

  case 236: /* cname: LTYPEDEF  */
#line 1174 "cc.y"
                 { (yyval.lval) = BTYPEDEF; }
#line 3585 "y.tab.c"
    break;

  case 237: /* cname: LTYPESTR  */
#line 1175 "cc.y"
                 { (yyval.lval) = BTYPESTR; }
#line 3591 "y.tab.c"
    break;

  case 238: /* cname: LREGISTER  */
#line 1176 "cc.y"
                  { (yyval.lval) = BREGISTER; }
#line 3597 "y.tab.c"
    break;

  case 239: /* cname: LINLINE  */
#line 1177 "cc.y"
                { (yyval.lval) = 0; }
#line 3603 "y.tab.c"
    break;

  case 240: /* gname: LCONSTNT  */
#line 1180 "cc.y"
                 { (yyval.lval) = BCONSTNT; }
#line 3609 "y.tab.c"
    break;

  case 241: /* gname: LVOLATILE  */
#line 1181 "cc.y"
                  { (yyval.lval) = BVOLATILE; }
#line 3615 "y.tab.c"
    break;

  case 242: /* gname: LRESTRICT  */
#line 1182 "cc.y"
                  { (yyval.lval) = 0; }
#line 3621 "y.tab.c"
    break;

  case 243: /* name: LNAME  */
#line 1186 "cc.y"
        {
		(yyval.node) = new(ONAME, Z, Z);
		if((yyvsp[0].sym)->class == CLOCAL)
			(yyvsp[0].sym) = mkstatic((yyvsp[0].sym));
		(yyval.node)->sym = (yyvsp[0].sym);
		(yyval.node)->type = (yyvsp[0].sym)->type;
		(yyval.node)->etype = TVOID;
		if((yyval.node)->type != T)
			(yyval.node)->etype = (yyval.node)->type->etype;
		(yyval.node)->xoffset = (yyvsp[0].sym)->offset;
		(yyval.node)->class = (yyvsp[0].sym)->class;
		(yyvsp[0].sym)->aused = 1;
	}
#line 3639 "y.tab.c"
    break;

  case 244: /* tag: ltag  */
#line 1201 "cc.y"
        {
		(yyval.node) = new(ONAME, Z, Z);
		(yyval.node)->sym = (yyvsp[0].sym);
		(yyval.node)->type = (yyvsp[0].sym)->type;
		(yyval.node)->etype = TVOID;
		if((yyval.node)->type != T)
			(yyval.node)->etype = (yyval.node)->type->etype;
		(yyval.node)->xoffset = (yyvsp[0].sym)->offset;
		(yyval.node)->class = (yyvsp[0].sym)->class;
	}
#line 3654 "y.tab.c"
    break;


#line 3658 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 1214 "cc.y"

