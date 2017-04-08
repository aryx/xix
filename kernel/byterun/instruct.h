/*s: byterun/instruct.h */
/*s: copyright header C xavier */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C xavier */

/*s: enum instructions */
/* The instruction set. */

enum instructions {

  /* integers */
  /*s: integer arithmetics opcodes */
  CONSTINT,
  /*x: integer arithmetics opcodes */
    CONST0, CONST1, CONST2, CONST3, 
    PUSHCONSTINT,
    PUSHCONST0, PUSHCONST1, PUSHCONST2, PUSHCONST3, 
  /*x: integer arithmetics opcodes */
  NEGINT, ADDINT, SUBINT, MULINT, DIVINT, MODINT,
  /*x: integer arithmetics opcodes */
  ANDINT, ORINT, XORINT, 
  /*x: integer arithmetics opcodes */
  LSLINT, LSRINT, ASRINT,
  /*x: integer arithmetics opcodes */
  EQ, NEQ, 
  /*x: integer arithmetics opcodes */
  LTINT, LEINT, GTINT, GEINT,
  /*x: integer arithmetics opcodes */
  OFFSETINT, OFFSETREF,
  /*e: integer arithmetics opcodes */

  /* stack, env, heap, data */
  /*s: basic stack operations opcodes */
  ACC,
  PUSH,
  POP, 
  ASSIGN,
  /*x: basic stack operations opcodes */
    ACC0, ACC1, ACC2, ACC3, ACC4, ACC5, ACC6, ACC7,
  /*x: basic stack operations opcodes */
    PUSHACC0, PUSHACC1, PUSHACC2, PUSHACC3,
    PUSHACC4, PUSHACC5, PUSHACC6, PUSHACC7,
  /*x: basic stack operations opcodes */
    PUSHACC, 
  /*e: basic stack operations opcodes */
  /*s: env access opcodes */
  ENVACC,
  /*x: env access opcodes */
    ENVACC1, ENVACC2, ENVACC3, ENVACC4, 
    PUSHENVACC1, PUSHENVACC2, PUSHENVACC3, PUSHENVACC4, PUSHENVACC,
  /*e: env access opcodes */
  /*s: blocks allocation opcodes */
  ATOM,
  MAKEBLOCK,
  /*x: blocks allocation opcodes */
    PUSHATOM,
    ATOM0, PUSHATOM0, 
    MAKEBLOCK1, MAKEBLOCK2, MAKEBLOCK3,
  /*e: blocks allocation opcodes */
  /*s: blocks access opcodes */
  GETFIELD,
  SETFIELD,
  /*x: blocks access opcodes */
    GETFIELD0, GETFIELD1, GETFIELD2, GETFIELD3,
    SETFIELD0, SETFIELD1, SETFIELD2, SETFIELD3,
  /*e: blocks access opcodes */
  /*s: global data access opcodes */
  GETGLOBAL, 
  SETGLOBAL, 
  /*x: global data access opcodes */
  GETGLOBALFIELD,
  /*x: global data access opcodes */
    PUSHGETGLOBAL, PUSHGETGLOBALFIELD, 
  /*e: global data access opcodes */

  /* control */
  /*s: branching opcodes */
  BRANCH, 
  BRANCHIF, 
  BRANCHIFNOT, 
  SWITCH, 
  BOOLNOT,
  /*e: branching opcodes */
  /*s: function application opcodes */
  PUSH_RETADDR, 
  APPLY, 
  APPTERM, 
  RETURN, 
  /*x: function application opcodes */
    APPLY1, APPLY2, APPLY3,
    APPTERM1, APPTERM2, APPTERM3, 
  /*e: function application opcodes */
  /*s: recursive definition opcodes */
  DUMMY, 
  UPDATE,
  /*e: recursive definition opcodes */
  /*s: exception opcodes */
  PUSHTRAP, 
  POPTRAP, 
  RAISE,
  /*e: exception opcodes */
  /*s: signal opcodes */
  CHECK_SIGNALS,
  /*e: signal opcodes */
  /*s: foreign C calls opcodes */
  C_CALLN,
  /*x: foreign C calls opcodes */
    C_CALL1, C_CALL2, C_CALL3, C_CALL4, C_CALL5,
  /*e: foreign C calls opcodes */

  /* special data */
  /*s: string opcodes */
  GETSTRINGCHAR, SETSTRINGCHAR, 
  /*e: string opcodes */
  /*s: array opcodes */
  VECTLENGTH, GETVECTITEM, SETVECTITEM,
  /*e: array opcodes */

  /* misc */
  /*s: misc opcodes */
  RESTART, 
  GRAB,
  CLOSURE, 
  CLOSUREREC,
  /*e: misc opcodes */
  /*s: debugger opcodes */
  STOP, EVENT, BREAK
  /*e: debugger opcodes */
};
/*e: enum instructions */
/*e: byterun/instruct.h */
