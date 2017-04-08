/*s: byterun/fail.h */
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

#ifndef _fail_
#define _fail_

#ifndef OS_PLAN9
#include <setjmp.h>
#else
#endif

#include "misc.h"
#include "mlvalues.h"

/*s: constant OUT_OF_MEMORY_EXN */
#define OUT_OF_MEMORY_EXN 0     /* "Out_of_memory" */
/*e: constant OUT_OF_MEMORY_EXN */
/*s: constant SYS_ERROR_EXN */
#define SYS_ERROR_EXN 1         /* "Sys_error" */
/*e: constant SYS_ERROR_EXN */
/*s: constant FAILURE_EXN */
#define FAILURE_EXN 2           /* "Failure" */
/*e: constant FAILURE_EXN */
/*s: constant INVALID_EXN */
#define INVALID_EXN 3           /* "Invalid_argument" */
/*e: constant INVALID_EXN */
/*s: constant END_OF_FILE_EXN */
#define END_OF_FILE_EXN 4       /* "End_of_file" */
/*e: constant END_OF_FILE_EXN */
/*s: constant ZERO_DIVIDE_EXN */
#define ZERO_DIVIDE_EXN 5       /* "Division_by_zero" */
/*e: constant ZERO_DIVIDE_EXN */
/*s: constant NOT_FOUND_EXN */
#define NOT_FOUND_EXN 6         /* "Not_found" */
/*e: constant NOT_FOUND_EXN */
/*s: constant MATCH_FAILURE_EXN */
#define MATCH_FAILURE_EXN 7     /* "Match_failure" */
/*e: constant MATCH_FAILURE_EXN */
/*s: constant STACK_OVERFLOW_EXN */
#define STACK_OVERFLOW_EXN 8    /* "Stack_overflow" */
/*e: constant STACK_OVERFLOW_EXN */

#ifdef POSIX_SIGNALS
/*s: struct longjmp_buffer */
struct longjmp_buffer {
  sigjmp_buf buf;
};
/*e: struct longjmp_buffer */
#else
struct longjmp_buffer {
  jmp_buf buf;
};
#define sigsetjmp(buf,save) setjmp(buf)
#define siglongjmp(buf,val) longjmp(buf,val)
#endif

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

void mlraise (value bucket) Noreturn;
void raise_constant (value tag) Noreturn;
void raise_with_arg (value tag, value arg) Noreturn;
void raise_with_string (value tag, char * msg) Noreturn;
void failwith (char *) Noreturn;
void invalid_argument (char *) Noreturn;
void raise_out_of_memory (void) Noreturn;
void raise_stack_overflow (void) Noreturn;
void raise_sys_error (value) Noreturn;
void raise_end_of_file (void) Noreturn;
void raise_zero_divide (void) Noreturn;
void raise_not_found (void) Noreturn;
void fatal_uncaught_exception (value) Noreturn;

#endif /* _fail_ */
/*e: byterun/fail.h */
