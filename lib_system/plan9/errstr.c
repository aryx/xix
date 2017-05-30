
#include <mlvalues.h>
#include "plan9support.h"

#include <lib9.h>

value plan9_errstr(value str, value len)               /* ML */
{
  int ret;
  ret = errstr(String_val(str), Int_val(len));
  if (ret != 0) p9error("errstr", Nothing);
  return Val_unit;
}
