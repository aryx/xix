
#include <mlvalues.h>
#include "plan9support.h"

#include <lib9.h>

value plan9_bind(value src, value dst, value flags)               /* ML */
{
  int ret;
  ret = bind(String_val(src), String_val(dst), Int_val(flags));
  if (ret < 0) p9error("bind", Nothing);
  return Val_int(ret);
}
