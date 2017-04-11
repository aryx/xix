/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* Stack backtrace for uncaught exceptions */

#include "config.h"

#ifndef OS_PLAN9
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#else
#endif

#include "mlvalues.h"
#include "alloc.h"
#include "io.h"
#include "instruct.h"
#include "intext.h"
#include "exec.h"
#include "fix_code.h"
#include "startup.h"
#include "stacks.h"
#include "sys.h"
#include "backtrace.h"

// bool
int backtrace_active = 0;

// value that was thrown in raise
value backtrace_last_exn = Val_unit;

int backtrace_pos = 0;
code_t * backtrace_buffer = NULL;
#define BACKTRACE_BUFFER_SIZE 1024

/* Location of fields in the Instruct.debug_event record */
enum { EV_POS = 0,
       EV_MODULE = 1,
       EV_CHAR = 2,
       EV_KIND = 3 };

/* Initialize the backtrace machinery */

void init_backtrace(void)
{
  backtrace_active = 1;
  register_global_root(&backtrace_last_exn);
  /* Note: lazy initialization of backtrace_buffer in stash_backtrace
     to simplify the interface with the thread libraries */
}

/* Store the return addresses contained in the given stack fragment
   into the backtrace array */

void stash_backtrace(value exn, code_t pc, value * sp)
{
  code_t end_code = (code_t) ((char *) start_code + code_size);
  if (pc != NULL) pc = pc - 1;
  // no need Kreraise; if catch and 'raise e' we will reraise with
  // the same exception value in which case we want the full backtrace
  if (exn != backtrace_last_exn) {
    backtrace_pos = 0;
    backtrace_last_exn = exn;
  }
  // lazy initialization of backtrace_buffer
  if (backtrace_buffer == NULL) {
    backtrace_buffer = malloc(BACKTRACE_BUFFER_SIZE * sizeof(code_t));
    if (backtrace_buffer == NULL) return;
  }
  if (backtrace_pos >= BACKTRACE_BUFFER_SIZE) return;
  backtrace_buffer[backtrace_pos++] = pc;
  for (/*nothing*/; sp < trapsp; sp++) {
    //todo: could be something other than a return address ...
    code_t p = (code_t) *sp;
    //todo: enough to make sure it's a pc?
    if (p >= start_code && p < end_code) {
      if (backtrace_pos >= BACKTRACE_BUFFER_SIZE) break;
      backtrace_buffer[backtrace_pos++] = p;
    }
  }
}

#ifndef OS_PLAN9
/* Read the debugging info contained in the current bytecode executable.
   Return a Caml array of Caml lists of debug_event records in "events",
   or Val_false on failure. */

static value read_debug_info(void)
{
  CAMLparam0();
  CAMLlocal1(events);
  char * exec_name;
  int fd;
  struct exec_trailer trail;
  struct channel * chan;
  uint32 num_events, orig, i;
  value evl, l;

  exec_name = caml_main_argv[0];
  fd = attempt_open(&exec_name, &trail, 1);
  if (fd < 0) CAMLreturn(Val_false);
  if (trail.debug_size == 0) {
    close(fd);
    CAMLreturn(Val_false);
  }

  lseek(fd, - (long) (TRAILER_SIZE + trail.debug_size), SEEK_END);
  chan = open_descriptor(fd);

  num_events = getword(chan);
  events = alloc(num_events, 0);
  for (i = 0; i < num_events; i++) {
    orig = getword(chan);
    evl = input_val(chan);
    /* Relocate events in event list */
    for (l = evl; l != Val_int(0); l = Field(l, 1)) {
      value ev = Field(l, 0);
      Field(ev, EV_POS) = Val_long(Long_val(Field(ev, EV_POS)) + orig);
    }
    /* Record event list */
    Store_field(events, i, evl);
  }
  close_channel(chan);
  CAMLreturn(events);
  return 0; // PAD
}
#endif

/* Search the event for the given PC.  Return Val_false if not found. */

static value event_for_location(value events, code_t pc)
{
  mlsize_t i;
  value pos, l, ev;

  Assert(pc >= start_code && pc < start_code + code_size);
  pos = Val_long((char *) pc - (char *) start_code);
  for (i = 0; i < Wosize_val(events); i++) {
    for (l = Field(events, i); l != Val_int(0); l = Field(l, 1)) {
      ev = Field(l, 0);
      if (Field(ev, EV_POS) == pos /* && Is_block(Field(ev, EV_KIND)) */)
        return ev;
    }
  }
  return Val_false;
}

/* Print the location corresponding to the given PC */

static void print_location(value events, int index)
{
  code_t pc = backtrace_buffer[index];
  char * info;
  value ev;

  if (pc == NULL) {
    fprintf(stderr, "Raised from a C function");
    return;
  }
  ev = event_for_location(events, pc);
  if (is_instruction(*pc, RAISE)) {
    /* Ignore compiler-inserted raise */
    if (ev == Val_false) return;

    /* Initial raise if index == 0, re-raise otherwise */
    if (index == 0)
      info = "Raised at";
    else
      info = "Re-raised at";
  } else {
    info = "Called from";
  }
  if (ev == Val_false) {
    fprintf(stderr, "%s unknown location\n", info);
  } else {
    fprintf(stderr, "%s module %s, character %d\n", info,
            String_val(Field(ev, EV_MODULE)),
            Int_val(Field(ev, EV_CHAR)));
  }
}

/* Print a backtrace */

void print_exception_backtrace(void)
{
  value events;
  int i;

#ifndef OS_PLAN9
  events = read_debug_info();
#else
  events = Val_false;
#endif
  if (events == Val_false) {
    fprintf(stderr,
            "(Program not linked with -g, cannot print stack backtrace)\n");
    return;
  }
  for (i = 0; i < backtrace_pos; i++)
    print_location(events, i);
}

/* Extract location information for the given PC */

struct loc_info {
  int loc_valid;
  int loc_is_raise;
  char * loc_modname;
  int loc_charpos;
};

static void extract_location_info(value events, code_t pc,
                                  /*out*/ struct loc_info * li)
{
  value ev;

  ev = event_for_location(events, pc);
  li->loc_is_raise = is_instruction(*pc, RAISE);
  if (ev == Val_false) {
    li->loc_valid = 0;
    return;
  }
  li->loc_valid = 1;

  li->loc_modname = String_val(Field(ev, EV_MODULE));
  li->loc_charpos = Int_val(Field(ev, EV_CHAR));
}


/* Convert the backtrace to a data structure usable from Caml */

value caml_get_exception_backtrace(value unit) /* ML */
{
#ifndef OS_PLAN9
  CAMLparam0();
  CAMLlocal5(events, res, arr, p, fname);
  int i;
  struct loc_info li;

  events = read_debug_info();
  if (events == Val_false) {
    res = Val_int(0);           /* None */
  } else {
    arr = alloc(backtrace_pos, 0);
    for (i = 0; i < backtrace_pos; i++) {

      if (backtrace_buffer[i] == NULL) {
        // raised from A function, do not call extract_location_info
        // otherwise you'll get a segfault if pass NULL
        p = alloc_small(1, 1);
        Field(p, 0) = Val_bool(0);
      } else {
      extract_location_info(events, backtrace_buffer[i], &li);
      if (li.loc_valid) {
        fname = copy_string(li.loc_modname);
        p = alloc_small(3, 0);
        Field(p, 0) = Val_bool(li.loc_is_raise);
        Field(p, 1) = fname;
        Field(p, 2) = Val_int(li.loc_charpos);
      } else {
        p = alloc_small(1, 1);
        Field(p, 0) = Val_bool(li.loc_is_raise);
      }
      }
      Modify(&Field(arr, i), p);
    }
    res = alloc_small(1, 0); 
    Field(res, 0) = arr; /* Some */
  }
  CAMLreturn(res);
#else
  return 0;
#endif

}
