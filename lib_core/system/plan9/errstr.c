
#ifdef OS_PLAN9_APE
#include <mlvalues.h>
#include "plan9support.h"
#else
#include <caml/mlvalues.h>
#include <caml/fail.h>
#endif

value plan9_errstr(value str, value len)               /* ML */
{
#ifdef OS_PLAN9_APE
  int ret;
  ret = errstr(String_val(str), Int_val(len));
  if (ret != 0) p9error("errstr");
  return Val_unit;
#else
  caml_failwith("plan9_errstr: valid only under Plan 9");
#endif
}
