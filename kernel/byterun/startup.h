#ifndef _startup_
#define _startup_

#include "misc.h"
#include "exec.h"

extern void caml_main(char **argv);
extern void caml_startup_code(code_t code, asize_t code_size,
                                  char *data, char **argv);

enum { FILE_NOT_FOUND = -1, BAD_BYTECODE  = -2 };

// used during startup (to load the bytecode) and during exit for the backtrace
extern int attempt_open(char **name, struct exec_trailer *trail,
                        int do_open_script);

#endif
