
#include <mlvalues.h>
#include "plan9support.h"

#include <lib9.h>

value plan9_bind(value str1, value str2, value flags)               /* ML */
{
  int ret;
  ret = bind(String_val(str1), String_val(str2), Int_val(flags));
  if (ret == -1) p9error("bind", Nothing);
  return Val_int(ret);
}
