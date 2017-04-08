/*s: byterun/terminfo.c */
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


/* Read and output terminal commands */

#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "mlvalues.h"

#ifdef HAS_TERMCAP

extern int tgetent (char * buffer, char * name);
extern char * tgetstr (char * id, char ** area);
extern int tgetnum (char * id);
extern int tputs (char * str, int count, int (*outchar)(int c));

/*s: function terminfo_setup */
value terminfo_setup(value unit)      /* ML */
{
  static char buffer[1024];
  if (tgetent(buffer, getenv("TERM")) != 1) failwith("Terminfo.setupterm");
  return Val_unit;
}
/*e: function terminfo_setup */

/*s: function terminfo_getstr */
value terminfo_getstr(value capa)     /* ML */
{
  char buff[1024];
  char * p = buff;
  char * s = tgetstr(String_val(capa), &p);
  if (s == NULL) raise_not_found();
  return copy_string(s);
}
/*e: function terminfo_getstr */

/*s: function terminfo_getnum */
value terminfo_getnum(value capa)     /* ML */
{
  int res = tgetnum(String_val(capa));
  if (res == -1) raise_not_found();
  return Val_int(res);
}
/*e: function terminfo_getnum */

/*s: global terminfo_putc_channel */
static struct channel * terminfo_putc_channel;
/*e: global terminfo_putc_channel */

/*s: function terminfo_putc */
static int terminfo_putc(int c)
{
  putch(terminfo_putc_channel, c);
  return c;
}
/*e: function terminfo_putc */

/*s: function terminfo_puts */
value terminfo_puts(value vchan, value str, value count) /* ML */
{
  terminfo_putc_channel = Channel(vchan);
  tputs(String_val(str), Int_val(count), terminfo_putc);
  return Val_unit;
}
/*e: function terminfo_puts */

#else

/*s: function terminfo_setup (byterun/terminfo.c) */
value terminfo_setup(value unit)
{
  failwith("Terminfo.setupterm");
  return Val_unit;
}
/*e: function terminfo_setup (byterun/terminfo.c) */

/*s: function terminfo_getstr (byterun/terminfo.c) */
value terminfo_getstr(value capa)
{
  raise_not_found();
  return Val_unit;
}
/*e: function terminfo_getstr (byterun/terminfo.c) */

/*s: function terminfo_getnum (byterun/terminfo.c) */
value terminfo_getnum(value capa)
{
  raise_not_found();
  return Val_unit;
}
/*e: function terminfo_getnum (byterun/terminfo.c) */

/*s: function terminfo_puts (byterun/terminfo.c) */
value terminfo_puts(value vchan, value str, value count)
{
  invalid_argument("Terminfo.puts");
  return Val_unit;
}
/*e: function terminfo_puts (byterun/terminfo.c) */

#endif
/*e: byterun/terminfo.c */
