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

/* The instruction set. */

enum instructions {

  /* integers */
  CONSTINT,
    CONST0, CONST1, CONST2, CONST3, 
    PUSHCONSTINT,
    PUSHCONST0, PUSHCONST1, PUSHCONST2, PUSHCONST3, 
  NEGINT, ADDINT, SUBINT, MULINT, DIVINT, MODINT,
  ANDINT, ORINT, XORINT, 
  LSLINT, LSRINT, ASRINT,
  EQ, NEQ, 
  LTINT, LEINT, GTINT, GEINT,
  OFFSETINT, OFFSETREF,

  /* stack, env, heap, data */
  ACC,
  PUSH,
  POP, 
  ASSIGN,
    ACC0, ACC1, ACC2, ACC3, ACC4, ACC5, ACC6, ACC7,
    PUSHACC0, PUSHACC1, PUSHACC2, PUSHACC3,
    PUSHACC4, PUSHACC5, PUSHACC6, PUSHACC7,
    PUSHACC, 
  ENVACC,
    ENVACC1, ENVACC2, ENVACC3, ENVACC4, 
    PUSHENVACC1, PUSHENVACC2, PUSHENVACC3, PUSHENVACC4, PUSHENVACC,
  ATOM,
  MAKEBLOCK,
    PUSHATOM,
    ATOM0, PUSHATOM0, 
    MAKEBLOCK1, MAKEBLOCK2, MAKEBLOCK3,
  GETFIELD,
  SETFIELD,
    GETFIELD0, GETFIELD1, GETFIELD2, GETFIELD3,
    SETFIELD0, SETFIELD1, SETFIELD2, SETFIELD3,
  GETGLOBAL, 
  SETGLOBAL, 
  GETGLOBALFIELD,
    PUSHGETGLOBAL, PUSHGETGLOBALFIELD, 

  /* control */
  BRANCH, 
  BRANCHIF, 
  BRANCHIFNOT, 
  SWITCH, 
  BOOLNOT,
  PUSH_RETADDR, 
  APPLY, 
  APPTERM, 
  RETURN, 
    APPLY1, APPLY2, APPLY3,
    APPTERM1, APPTERM2, APPTERM3, 
  DUMMY, 
  UPDATE,
  PUSHTRAP, 
  POPTRAP, 
  RAISE,
  CHECK_SIGNALS,
  C_CALLN,
    C_CALL1, C_CALL2, C_CALL3, C_CALL4, C_CALL5,

  /* special data */
  GETSTRINGCHAR, SETSTRINGCHAR, 
  VECTLENGTH, GETVECTITEM, SETVECTITEM,

  /* misc */
  RESTART, 
  GRAB,
  CLOSURE, 
  CLOSUREREC,
  STOP, EVENT, BREAK
};
