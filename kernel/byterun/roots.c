/*s: byterun/roots.c */
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

/* To walk the memory roots for garbage collection */

#include "memory.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stacks.h"

/*s: global local_roots */
struct caml__roots_block *local_roots = NULL;
/*e: global local_roots */

/*s: struct global_root */
struct global_root {
  value * root;
  struct global_root * next;
};
/*e: struct global_root */

/*s: global global_roots */
static struct global_root * global_roots = NULL;
/*e: global global_roots */

/*s: global scan_roots_hook */
void (*scan_roots_hook) (scanning_action f) = NULL;
/*e: global scan_roots_hook */

/*s: function register_global_root */
/* Register a global C root */

void register_global_root(value *r)
{
  struct global_root * gr;
  gr = (struct global_root *) stat_alloc(sizeof(struct global_root));
  gr->root = r;
  gr->next = global_roots;
  global_roots = gr;
}
/*e: function register_global_root */

/*s: function remove_global_root */
/* Un-register a global C root */

void remove_global_root(value *r)
{
  struct global_root ** gp, * gr;
  for (gp = &global_roots; *gp != NULL; gp = &(*gp)->next) {
    gr = *gp;
    if (gr->root == r) {
      *gp = gr->next;
      stat_free(gr);
      return;
    }
  }
}
/*e: function remove_global_root */

/*s: function oldify_local_roots */
/* Call [oldify] on all roots except [global_data] */

void oldify_local_roots (void)
{
  register value * sp;
  struct global_root * gr;
  struct caml__roots_block *lr;
  long i, j;

  /* The stack */
  for (sp = extern_sp; sp < stack_high; sp++) {
    oldify (*sp, sp);
  }
  /* Local C roots */
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        oldify (*sp, sp);
      }
    }
  }
  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    oldify(*(gr->root), gr->root);
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(oldify);
}
/*e: function oldify_local_roots */

/*s: function darken_all_roots */
/* Call [darken] on all roots */

void darken_all_roots (void)
{
  do_roots (darken);
}
/*e: function darken_all_roots */

/*s: function do_roots */
void do_roots (scanning_action f)
{
  struct global_root * gr;

  /* Global variables */
  f(global_data, &global_data);

  /* The stack and the local C roots */
  do_local_roots(f, extern_sp, stack_high, local_roots);

  /* Global C roots */
  for (gr = global_roots; gr != NULL; gr = gr->next) {
    f (*(gr->root), gr->root);
  }
  /* Hook */
  if (scan_roots_hook != NULL) (*scan_roots_hook)(f);
}
/*e: function do_roots */

/*s: function do_local_roots */
void do_local_roots (scanning_action f, value *stack_low, value *stack_high, struct caml__roots_block *local_roots)
{
  register value * sp;
  struct caml__roots_block *lr;
  int i, j;

  for (sp = stack_low; sp < stack_high; sp++) {
    f (*sp, sp);
  }
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        f (*sp, sp);
      }
    }
  }
}
/*e: function do_local_roots */

/*e: byterun/roots.c */
