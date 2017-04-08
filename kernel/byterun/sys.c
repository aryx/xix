/*s: byterun/sys.c */
/*s: copyright header C xavier */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C xavier */

/* Basic system calls */

#include "config.h"

#ifndef OS_PLAN9
#include <errno.h>
// O_NONBLOCK
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#else
#define open myopen
#endif

#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "str.h"
#include "sys.h"

extern int errno;

extern char * strerror(int);

/*s: function error_message */
char * error_message(void)
{
  return strerror(errno);
}
/*e: function error_message */

/*s: function sys_error */
void sys_error(value arg)
{
  char * err = error_message();
  value str;
  
  if (arg == NO_ARG) {
    str = copy_string(err);
  } else {
    int err_len = strlen(err);
    int arg_len = string_length(arg);
    Begin_root(arg);
      str = alloc_string(arg_len + 2 + err_len);
    End_roots();
    bcopy(String_val(arg), &Byte(str, 0), arg_len);
    bcopy(": ", &Byte(str, arg_len), 2);
    bcopy(err, &Byte(str, arg_len + 2), err_len);
  }
  raise_sys_error(str);
}
/*e: function sys_error */

/*s: function sys_exit */
value sys_exit(value retcode)          /* ML */
{
#ifndef NATIVE_CODE
  debugger(PROGRAM_EXIT);
#endif
  exit(Int_val(retcode));
  return Val_unit;
}
/*e: function sys_exit */

/*s: global sys_open_flags */
static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_APPEND, O_CREAT, O_TRUNC, O_EXCL, O_NONBLOCK
};
/*e: global sys_open_flags */

/*s: function sys_open */
value sys_open(value path, value flags, value perm) /* ML */
{
  int ret;
  ret = open(String_val(path), convert_flag_list(flags, sys_open_flags), 
             Int_val(perm));
  if (ret == -1) sys_error(path);
  return Val_long(ret);
}
/*e: function sys_open */

/*s: function sys_close */
value sys_close(value fd)             /* ML */
{
  close(Int_val(fd));
  return Val_unit;
}
/*e: function sys_close */

/*s: function sys_file_exists */
value sys_file_exists(value name)     /* ML */
{
#ifndef OS_PLAN9
  struct stat st;
  return Val_bool(stat(String_val(name), &st) == 0);
#else
  return Val_false;
#endif
}
/*e: function sys_file_exists */

/*s: function sys_remove */
value sys_remove(value name)          /* ML */
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(name);
  return Val_unit;
}
/*e: function sys_remove */

/*s: function sys_rename */
value sys_rename(value oldname, value newname) /* ML */
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(oldname);
  return Val_unit;
}
/*e: function sys_rename */

/*s: function sys_chdir */
value sys_chdir(value dirname)        /* ML */
{
  if (chdir(String_val(dirname)) != 0) sys_error(dirname);
  return Val_unit;
}
/*e: function sys_chdir */

/*s: function sys_getcwd */
value sys_getcwd(value unit)          /* ML */
{
  char buff[4096];
  if (getcwd(buff, sizeof(buff)) == 0) sys_error(NO_ARG);
  return copy_string(buff);
}
/*e: function sys_getcwd */

/*s: function sys_getenv */
value sys_getenv(value var)           /* ML */
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) raise_not_found();
  return copy_string(res);
}
/*e: function sys_getenv */

/*s: global main_argv */
char ** caml_main_argv;
/*e: global main_argv */

/*s: function sys_get_argv */
value sys_get_argv(value unit)        /* ML */
{
  return copy_string_array(caml_main_argv);
}
/*e: function sys_get_argv */

/*s: function sys_init */
void sys_init(char **argv)
{
  caml_main_argv = argv;
}
/*e: function sys_init */

/*s: function sys_system_command */
value sys_system_command(value command)   /* ML */
{
  int retcode = system(String_val(command));
  if (retcode == -1) sys_error(command);
  return Val_int(retcode);
}
/*e: function sys_system_command */

/*s: function sys_get_config */
value sys_get_config(value unit)  /* ML */
{
  value result;
  value ostype;

  ostype = copy_string(OCAML_OS_TYPE);
  Begin_root(ostype);
    result = alloc_small (2, 0);
    Field(result, 0) = ostype;
    Field(result, 1) = Val_long (8 * sizeof(value));
  End_roots ();
  return result;
}
/*e: function sys_get_config */

/* Search path function */

#ifndef S_ISREG
/*s: function S_ISREG */
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
/*e: function S_ISREG */
#endif

/*s: function searchpath */
char * searchpath(char * name)
{
  char * fullname;
  char * path;
  char * p;
  char * q;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == NULL) return 0;
  fullname = stat_alloc(strlen(name) + strlen(path) + 2);
  while(1) {
    for (p = fullname; *path != 0 && *path != ':'; p++, path++) *p = *path;
    if (p != fullname) *p++ = '/';
    for (q = name; *q != 0; p++, q++) *p = *q;
    *p = 0;
#ifndef OS_PLAN9
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) break;
#else
    return 0;
#endif
    if (*path == 0) return 0;
    path++;
  }
  return fullname;
}
/*e: function searchpath */
/*e: byterun/sys.c */
