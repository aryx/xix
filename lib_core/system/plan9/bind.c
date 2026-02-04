
#ifdef OS_PLAN9_APE
#include <mlvalues.h>
#include "plan9support.h"
#else
#include <caml/mlvalues.h>
#include <caml/fail.h>
#endif

extern	int	_BIND(char*, char*, int);

value plan9_bind(value src, value dst, value flags)               /* ML */
{
#ifdef OS_PLAN9_APE
  int ret;
  // can't call bind() from lib9.h because bind() is also defined
  // in APE/lib/bsd/bind.c but the linker does not say anything
  // and we link both libbsd.a and lib9.a because of the use of socket
  // in ocaml-light-/otherlib/unix/
  // So call directly the APE syscall _BIND, simpler
  ret = _BIND(String_val(src), String_val(dst), Int_val(flags));
  //printf("bind: %s, %s, %d, ret = %d\n", String_val(src), String_val(dst), Int_val(flags), ret);
  //ret = _BIND("/tests/bytecode", "/tests/test", 0);
  if (ret < 0) p9error("bind");
  return Val_int(ret);
#else
  caml_failwith("plan9_bind: valid only under Plan 9");
#endif
}
