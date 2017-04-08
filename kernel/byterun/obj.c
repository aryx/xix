/*s: byterun/obj.c */
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

/* Operations on objects */

#include "alloc.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"

/*s: function static_alloc */
value static_alloc(value size)        /* ML */
{
  return (value) stat_alloc((asize_t) Long_val(size));
}
/*e: function static_alloc */

/*s: function static_free */
value static_free(value blk)          /* ML */
{
  stat_free((void *) blk);
  return Val_unit;
}
/*e: function static_free */

/*s: function static_resize */
value static_resize(value blk, value new_size) /* ML */
{
  return (value) stat_resize((char *) blk, (asize_t) Long_val(new_size));
}
/*e: function static_resize */

/*s: function obj_is_block */
value obj_is_block(value arg)             /* ML */
{
  return Val_bool(Is_block(arg));
}
/*e: function obj_is_block */

/*s: function obj_tag */
value obj_tag(value arg)                 /* ML */
{
  return Val_int(Tag_val(arg));
}
/*e: function obj_tag */

/*s: function obj_block */
value obj_block(value tag, value size) /* ML */
{
  value res;
  mlsize_t sz, i;
  tag_t tg;

  sz = Long_val(size);
  tg = Long_val(tag);
  if (sz == 0) return Atom(tg);
  res = alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = Val_long(0);

  return res;
}
/*e: function obj_block */

/*e: byterun/obj.c */
