/*s: byterun/prims.h */
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

/* Interface with C primitives. */

#ifndef _prims_
#define _prims_

typedef value (*c_primitive)();

extern c_primitive cprim[];
extern char * names_of_cprim[];

#endif /* _prims_ */
/*e: byterun/prims.h */
