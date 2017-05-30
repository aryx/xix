#include <mlvalues.h>
#include <alloc.h>
#include <callback.h>
#include <memory.h>
#include <fail.h>

#include "plan9support.h"

static value * plan9_error_exn = NULL;

void plan9_error(int errcode, char *cmdname, value cmdarg)
{
  value res;
  value name = Val_unit, arg = Val_unit;

  Begin_roots2 (name, arg);
    if (plan9_error_exn == NULL) {
      plan9_error_exn = caml_named_value("Plan9.Plan9_error");
      if (plan9_error_exn == NULL)
        invalid_argument("Exception Plan9.Plan9_error not initialized, please link plan9.cma");
    }
    arg = cmdarg == Nothing ? copy_string("") : cmdarg;
    name = copy_string(cmdname);
    res = alloc(4, 0);
    Field(res, 0) = *plan9_error_exn;
    Field(res, 1) = Val_int(errcode);
//      cst_to_constr(errcode, error_table, sizeof(error_table)/sizeof(int),
//                    sizeof(error_table)/sizeof(int));
    Field(res, 2) = name;
    Field(res, 3) = arg;
  End_roots();
  mlraise(res);
}

//TODO: use errstr! 
void p9error(char *cmdname, value cmdarg)
{
  plan9_error(1/*errno*/, cmdname, cmdarg);
}

