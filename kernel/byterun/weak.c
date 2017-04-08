/*s: byterun/weak.c */
/*s: copyright header C damien 1997 */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C damien 1997 */

/* Operations on weak arrays */

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

/*s: global weak_list_head */
value weak_list_head = 0;
/*e: global weak_list_head */

/*s: function weak_create */
value weak_create (value len)        /* ML */
{
  mlsize_t size, i;
  value res;

  size = Long_val (len) + 1;
  if (size > Max_wosize) invalid_argument ("Weak.create");
  res = alloc_shr (size, Abstract_tag);
  for (i = 1; i < size; i++) Field (res, i) = 0;
  Field (res, 0) = weak_list_head;
  weak_list_head = res;
  return res;
}
/*e: function weak_create */

/*s: constant None_val */
#define None_val 1
/*e: constant None_val */
/*s: constant Some_tag */
#define Some_tag 0
/*e: constant Some_tag */

/*s: function weak_set */
value weak_set (value ar, value n, value el)     /* ML */
{
  mlsize_t offset = Long_val (n) + 1;
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.set");
  Field (ar, offset) = 0;
  if (el != None_val){                  Assert (Wosize_val (el) == 1);
    Modify (&Field (ar, offset), Field (el, 0));
  }
  return Val_unit;
}
/*e: function weak_set */

/*s: constant Setup_for_gc (byterun/weak.c) */
#define Setup_for_gc
/*e: constant Setup_for_gc (byterun/weak.c) */
/*s: constant Restore_after_gc (byterun/weak.c) */
#define Restore_after_gc
/*e: constant Restore_after_gc (byterun/weak.c) */

/*s: function weak_get */
value weak_get (value ar, value n)        /* ML */
{
  mlsize_t offset = Long_val (n) + 1;
  value res;
  value elt;
                                                   Assert (Is_in_heap (ar));
  if (offset < 1 || offset >= Wosize_val (ar)) invalid_argument ("Weak.get");
  if (Field (ar, offset) == 0){
    res = None_val;
  }else{
    elt = Field (ar, offset);
    if (gc_phase == Phase_mark) darken (elt, NULL);
    Begin_root(elt);
      res = alloc_small (1, Some_tag);
    End_roots ();
    Field (res, 0) = elt;
  }
  return res;
}
/*e: function weak_get */

#undef Setup_for_gc
#undef Restore_after_gc
/*e: byterun/weak.c */
