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

/* Print an uncaught exception and abort */

#include "config.h"

#ifndef OS_PLAN9
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#else
#endif

#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "backtrace.h"
#include "debugger.h"

struct stringbuf {
  char * ptr;
  char * end;
  char data[256];
};

static void add_char(struct stringbuf *buf, char c)
{
  if (buf->ptr < buf->end) *(buf->ptr++) = c;
}

static void add_string(struct stringbuf *buf, char *s)
{
  int len = strlen(s);
  if (buf->ptr + len > buf->end) len = buf->end - buf->ptr;
  if (len > 0) bcopy(s, buf->ptr, len);
  buf->ptr += len;
}
  
#define errprintf(fmt,arg) fprintf(stderr, fmt, arg)

void fatal_uncaught_exception(value exn)
{
  mlsize_t start, i;
  value bucket, v;
  struct stringbuf buf;
  char intbuf[64];

  buf.ptr = buf.data;
  buf.end = buf.data + sizeof(buf.data) - 1;
  add_string(&buf, String_val(Field(Field(exn, 0), 0)));
  if (Wosize_val(exn) >= 2) {
    /* Check for exceptions in the style of Match_failure and Assert_failure */
    if (Wosize_val(exn) == 2 &&
        Is_block(Field(exn, 1)) &&
        Tag_val(Field(exn, 1)) == 0) {
      bucket = Field(exn, 1);
      start = 0;
    } else {
      bucket = exn;
      start = 1;
    }
    add_char(&buf, '(');
    for (i = start; i < Wosize_val(bucket); i++) {
      if (i > start) add_string(&buf, ", ");
      v = Field(bucket, i);
      if (Is_long(v)) {
        sprintf(intbuf, "%ld", Long_val(v));
        add_string(&buf, intbuf);
      } else if (Tag_val(v) == String_tag) {
        add_char(&buf, '"');
        add_string(&buf, String_val(v));
        add_char(&buf, '"');
      } else {
        add_char(&buf, '_');
      }
    }
    add_char(&buf, ')');
  }
  *buf.ptr = 0;              /* Terminate string */
  errprintf("Fatal error: uncaught exception %s\n", buf.data);
  if (backtrace_active && !debugger_in_use) print_exception_backtrace();

  exit(2);
}
