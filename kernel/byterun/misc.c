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

#include "config.h"

#ifndef OS_PLAN9
#include <stdio.h>
#else
#endif

#include "misc.h"

#ifdef DEBUG

void failed_assert (char * expr, char * file, int line)
{
  fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
           expr, file, line);
  exit (100);
}

static unsigned long seed = 0x12345;

unsigned long not_random (void)
{
  seed = seed * 65537 + 12345;
  return seed;
}

#endif

int verb_gc;

void gc_message (char *msg, long unsigned int arg)
{
  if (verb_gc){
    fprintf (stderr, msg, arg);
    fflush (stderr);
  }
}

void fatal_error (char *msg)
{
  fprintf (stderr, "%s", msg);
  exit(2);
}

void fatal_error_arg (char *fmt, char *arg)
{
  fprintf (stderr, fmt, arg);
  exit(2);
}

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
