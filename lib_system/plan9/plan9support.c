#include <mlvalues.h>
#include <alloc.h>
#include <callback.h>
#include <memory.h>
#include <fail.h>

#include "plan9support.h"

#include <lib9.h>

static value * plan9_error_exn = NULL;

void plan9_error(char *cmdname, value cmdarg)
{
  value res;
  value name = Val_unit, arg = Val_unit;

  Begin_roots2 (name, arg);
    if (plan9_error_exn == NULL) {
      plan9_error_exn = caml_named_value("Plan9.Plan9_error");
      if (plan9_error_exn == NULL)
        invalid_argument("Exception Plan9.Plan9_error not initialized, please link plan9.cma");
    }
    arg = (cmdarg == Nothing) ? copy_string("") : cmdarg;
    name = copy_string(cmdname);
    res = alloc(3, 0);
    Field(res, 0) = *plan9_error_exn;
    Field(res, 1) = name;
    Field(res, 2) = arg;
  End_roots();
  mlraise(res);
}

void p9error(char *cmdname)
{
  char buf[ERRMAX];
  buf[0] = '\0';
  errstr(buf, ERRMAX);
  plan9_error(cmdname, copy_string(buf));
}
