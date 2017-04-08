/*s: byterun/interp.h */
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

/* The bytecode interpreter */

#ifndef _interp_
#define _interp_

#include "misc.h"
#include "mlvalues.h"

/*s: signature function interprete */
value interprete (code_t prog, asize_t prog_size);
/*e: signature function interprete */

#endif
/*e: byterun/interp.h */
