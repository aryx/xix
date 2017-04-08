/*s: byterun/callback.c */
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

/* Callbacks from C to Caml */

#include "config.h"

#ifndef OS_PLAN9
#include <string.h>
#else
#endif

#include "callback.h"
#include "memory.h"
#include "mlvalues.h"

/* Bytecode callbacks (implemented in asm for the native code compiler) */

#ifndef NATIVE_CODE

#include "interp.h"
#include "instruct.h"
#include "fix_code.h"
#include "stacks.h"

/*s: global callback_depth */
int callback_depth = 0;
/*e: global callback_depth */

/*s: global callback1_code */
static opcode_t callback1_code[] = { ACC1, APPLY1, POP, 1, STOP };
/*e: global callback1_code */
/*s: global callback2_code */
static opcode_t callback2_code[] = { ACC2, APPLY2, POP, 1, STOP };
/*e: global callback2_code */
/*s: global callback3_code */
static opcode_t callback3_code[] = { ACC3, APPLY3, POP, 1, STOP };
/*e: global callback3_code */

#ifdef THREADED_CODE

/*s: global callback_code_threaded */
static int callback_code_threaded = 0;
/*e: global callback_code_threaded */

/*s: function thread_callback */
static void thread_callback(void)
{
  thread_code(callback1_code, sizeof(callback1_code));
  thread_code(callback2_code, sizeof(callback2_code));
  thread_code(callback3_code, sizeof(callback3_code));
  callback_code_threaded = 1;
}
/*e: function thread_callback */

#define Init_callback() if (!callback_code_threaded) thread_callback()

#else

/*s: function Init_callback */
#define Init_callback()
/*e: function Init_callback */

#endif

/*s: function callback */
value callback(value closure, value arg)
{
  value res;
  Init_callback();
  extern_sp -= 2;
  extern_sp[0] = arg;
  extern_sp[1] = closure;
  callback_depth++;
  res = interprete(callback1_code, sizeof(callback1_code));
  callback_depth--;
  return res;
}
/*e: function callback */

/*s: function callback2 */
value callback2(value closure, value arg1, value arg2)
{
  value res;
  Init_callback();
  extern_sp -= 3;
  extern_sp[0] = arg1;
  extern_sp[1] = arg2;
  extern_sp[2] = closure;
  callback_depth++;
  res = interprete(callback2_code, sizeof(callback2_code));
  callback_depth--;
  return res;
}
/*e: function callback2 */

/*s: function callback3 */
value callback3(value closure, value arg1, value arg2, value arg3)
{
  value res;
  Init_callback();
  extern_sp -= 4;
  extern_sp[0] = arg1;
  extern_sp[1] = arg2;
  extern_sp[2] = arg3;
  extern_sp[3] = closure;
  callback_depth++;
  res = interprete(callback3_code, sizeof(callback3_code));
  callback_depth--;
  return res;
}
/*e: function callback3 */

#endif

/*s: struct named_value */
/* Naming of Caml values */

struct named_value {
  value val;
  struct named_value * next;
  char name[1];
};
/*e: struct named_value */

/*s: constant Named_value_size */
#define Named_value_size 13
/*e: constant Named_value_size */

/*s: global named_value_table */
static struct named_value * named_value_table[Named_value_size] = { NULL, };
/*e: global named_value_table */

/*s: function hash_value_name */
static unsigned int hash_value_name(char *name)
{
  unsigned int h;
  for (h = 0; *name != 0; name++) h = h * 19 + *name;
  return h % Named_value_size;
}
/*e: function hash_value_name */

/*s: function register_named_value */
value register_named_value(value vname, value val) /* ML */
{
  struct named_value * nv;
  char * name = String_val(vname);
  unsigned int h = hash_value_name(name);

  nv = (struct named_value *)
         stat_alloc(sizeof(struct named_value) + strlen(name));
  strcpy(nv->name, name);
  nv->val = val;
  nv->next = named_value_table[h];
  named_value_table[h] = nv;
  register_global_root(&nv->val);
  return Val_unit;
}
/*e: function register_named_value */

/*s: function caml_named_value */
value * caml_named_value(char *name)
{
  struct named_value * nv;
  for (nv = named_value_table[hash_value_name(name)];
       nv != NULL;
       nv = nv->next) {
    if (strcmp(name, nv->name) == 0) return &nv->val;
  }
  return NULL;
}
/*e: function caml_named_value */
/*e: byterun/callback.c */
