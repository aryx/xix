/*s: byterun/misc.c */
/*s: copyright header C xavier and damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C xavier and damien */

#include "config.h"

#ifndef OS_PLAN9
#include <stdio.h>
#else
#endif

#include "misc.h"

#ifdef DEBUG

/*s: function failed_assert */
void failed_assert (char * expr, char * file, int line)
{
  fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
           expr, file, line);
  exit (100);
}
/*e: function failed_assert */

/*s: global seed */
static unsigned long seed = 0x12345;
/*e: global seed */

/*s: function not_random */
unsigned long not_random (void)
{
  seed = seed * 65537 + 12345;
  return seed;
}
/*e: function not_random */

#endif

/*s: global verb_gc */
int verb_gc;
/*e: global verb_gc */

/*s: function gc_message */
void gc_message (char *msg, long unsigned int arg)
{
  if (verb_gc){
    fprintf (stderr, msg, arg);
    fflush (stderr);
  }
}
/*e: function gc_message */

/*s: function fatal_error */
void fatal_error (char *msg)
{
  fprintf (stderr, "%s", msg);
  exit(2);
}
/*e: function fatal_error */

/*s: function fatal_error_arg */
void fatal_error_arg (char *fmt, char *arg)
{
  fprintf (stderr, fmt, arg);
  exit(2);
}
/*e: function fatal_error_arg */

/*s: function aligned_malloc */
char *aligned_malloc (asize_t size, int modulo, void **block)
                  
                
                        /* output */
{
  char *raw_mem;
  unsigned long aligned_mem;
                                                 Assert (modulo < Page_size);
  raw_mem = (char *) malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
  return (char *) (aligned_mem - modulo);
}
/*e: function aligned_malloc */
/*e: byterun/misc.c */
