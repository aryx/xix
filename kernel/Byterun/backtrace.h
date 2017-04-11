#ifndef _backtrace_
#define _backtrace_

#include "mlvalues.h"

extern int backtrace_active;
extern int backtrace_pos;
extern code_t * backtrace_buffer;
extern value backtrace_last_exn;

extern void init_backtrace(void);
extern void stash_backtrace(value exn, code_t pc, value * sp);
extern void print_exception_backtrace(void);

#endif
