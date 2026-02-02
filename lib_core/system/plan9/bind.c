
#ifdef OS_PLAN9_APE
#include <mlvalues.h>
#include "plan9support.h"
#else
#include <caml/mlvalues.h>
#include <caml/fail.h>
#endif

value plan9_bind(value src, value dst, value flags)               /* ML */
{
#ifdef OS_PLAN9_APE
  int ret;
  ret = bind(String_val(src), String_val(dst), Int_val(flags));
  if (ret < 0) p9error("bind");
  return Val_int(ret);
#else
  caml_failwith("plan9_bind: valid only under Plan 9");
#endif
}
