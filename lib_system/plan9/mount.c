
#include <mlvalues.h>
#include "plan9support.h"

#include <lib9.h>

value plan9_mount(value fd, value int1, value dst, value flags, value args)               /* ML */
{
  int ret;
  ret = mount(Int_val(fd), Int_val(int1), String_val(dst), Int_val(flags),
              String_val(args));
  if (ret < 0) p9error("mount", Nothing);
  return Val_int(ret);
}

