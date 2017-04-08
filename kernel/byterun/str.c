/*s: byterun/str.c */
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

/* Operations on strings */

#include "config.h"

#ifndef OS_PLAN9
#include <string.h>
#endif

#include "alloc.h"
#include "fail.h"
#include "mlvalues.h"
#include "misc.h"

/*s: function string_length */
mlsize_t string_length(value s)
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}
/*e: function string_length */

/*s: function ml_string_length */
value ml_string_length(value s)     /* ML */
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}
/*e: function ml_string_length */

/*s: function create_string */
value create_string(value len)        /* ML */
{
  mlsize_t size = Long_val(len);
  if (size > Bsize_wsize (Max_wosize) - 1) invalid_argument("String.create");
  return alloc_string(size);
}
/*e: function create_string */

/*s: function string_get */
value string_get(value str, value index)    /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= string_length(str)) invalid_argument("String.get");
  return Val_int(Byte_u(str, idx));
}
/*e: function string_get */

/*s: function string_set */
value string_set(value str, value index, value newval)    /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= string_length(str)) invalid_argument("String.set");
  Byte_u(str, idx) = Int_val(newval);
  return Val_unit;
}
/*e: function string_set */

/*s: function string_equal */
value string_equal(value s1, value s2)      /* ML */
{
  mlsize_t sz1 = Wosize_val(s1);
  mlsize_t sz2 = Wosize_val(s2);
  value * p1, * p2;
  if (sz1 != sz2) return Val_false;
  for(p1 = Op_val(s1), p2 = Op_val(s2); sz1 > 0; sz1--, p1++, p2++)
    if (*p1 != *p2) return Val_false;
  return Val_true;
}
/*e: function string_equal */

/*s: function string_notequal */
value string_notequal(value s1, value s2)   /* ML */
{
  return Val_not(string_equal(s1, s2));
}
/*e: function string_notequal */
  
/*s: function blit_string */
value blit_string(value s1, value ofs1, value s2, value ofs2, value n)   /* ML */
{
  bcopy(&Byte(s1, Long_val(ofs1)), &Byte(s2, Long_val(ofs2)), Int_val(n));
  return Val_unit;
}
/*e: function blit_string */

/*s: function fill_string */
value fill_string(value s, value offset, value len, value init) /* ML */
{
  register char * p;
  register mlsize_t n;
  register char c;

  c = Long_val(init);
  for(p = &Byte(s, Long_val(offset)), n = Long_val(len);
      n > 0; n--, p++)
    *p = c;
  return Val_unit;
}
/*e: function fill_string */

/*s: global printable_chars_ascii */
static unsigned char printable_chars_ascii[] = /* 0x20-0x7E */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
/*e: global printable_chars_ascii */
/*s: global printable_chars_iso */
static unsigned char printable_chars_iso[] = /* 0x20-0x7E 0xA1-0xFF */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\376\377\377\377\377\377\377\377\377\377\377\377";
/*e: global printable_chars_iso */

/*s: function is_printable */
value is_printable(value chr) /* ML */
{
  int c;
  unsigned char * printable_chars;

  static int iso_charset = -1;
  if (iso_charset == -1) {
    char * lc_ctype = (char *) getenv("LC_CTYPE");
    iso_charset = (lc_ctype != 0 && strcmp(lc_ctype, "iso_8859_1") == 0);
  }
  printable_chars = iso_charset ? printable_chars_iso : printable_chars_ascii;
  c = Int_val(chr);
  return Val_bool(printable_chars[c >> 3] & (1 << (c & 7)));
}
/*e: function is_printable */

/*s: function bitvect_test */
value bitvect_test(value bv, value n)       /* ML */
{
  int pos = Int_val(n);
  return Val_int(Byte_u(bv, pos >> 3) & (1 << (pos & 7)));
}
/*e: function bitvect_test */

/*e: byterun/str.c */
