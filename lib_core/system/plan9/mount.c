
#ifdef OS_PLAN9_APE
#include <mlvalues.h>
#include "plan9support.h"
#else
#include <caml/mlvalues.h>
#include <caml/fail.h>
#endif

value plan9_mount(value fd, value int1, value dst, value flags, value args) /* ML */
{
#ifdef OS_PLAN9_APE
  int ret;
  ret = mount(Int_val(fd), Int_val(int1), String_val(dst), Int_val(flags),
              String_val(args));
  if (ret < 0) p9error("mount");
  return Val_int(ret);
#else
  caml_failwith("plan9_mount: valid only under Plan 9");
#endif
}

