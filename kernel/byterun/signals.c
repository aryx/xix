/*s: byterun/signals.c */
/*s: copyright header C xavier and damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C xavier and damien */

#include "config.h"

#ifndef OS_PLAN9
#include <signal.h>
#else
#endif

#include "alloc.h"
#include "callback.h"
#include "config.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"

/*s: global async_signal_mode */
volatile int async_signal_mode = 0;
/*e: global async_signal_mode */
/*s: global pending_signal */
volatile int pending_signal = 0;
/*e: global pending_signal */
/*s: global something_to_do */
volatile int something_to_do = 0;
/*e: global something_to_do */
/*s: global force_major_slice */
volatile int force_major_slice = 0;
/*e: global force_major_slice */
/*s: global signal_handlers */
value signal_handlers = 0;
/*e: global signal_handlers */
/*s: global enter_blocking_section_hook */
void (*enter_blocking_section_hook)() = NULL;
/*e: global enter_blocking_section_hook */
/*s: global leave_blocking_section_hook */
void (*leave_blocking_section_hook)() = NULL;
/*e: global leave_blocking_section_hook */

/*s: function execute_signal */
static void execute_signal(int signal_number)
{
  Assert (!async_signal_mode);
  callback(Field(signal_handlers, signal_number), Val_int(signal_number));
}
/*e: function execute_signal */

/*s: function handle_signal */
void handle_signal(int signal_number)
{
#ifndef POSIX_SIGNALS
#ifndef BSD_SIGNALS
  signal(signal_number, handle_signal);
#endif
#endif
  if (async_signal_mode){
    leave_blocking_section ();
    execute_signal(signal_number);
    enter_blocking_section ();
  }else{
    pending_signal = signal_number;
    something_to_do = 1;
  }
}
/*e: function handle_signal */

/*s: function urge_major_slice */
void urge_major_slice (void)
{
  force_major_slice = 1;
  something_to_do = 1;
}
/*e: function urge_major_slice */

/*s: function enter_blocking_section */
void enter_blocking_section(void)
{
  int temp;

  while (1){
    Assert (!async_signal_mode);
    /* If a signal arrives between the next two instructions,
       it will be lost. */
    temp = pending_signal;   pending_signal = 0;
    if (temp) execute_signal(temp);
    async_signal_mode = 1;
    if (!pending_signal) break;
    async_signal_mode = 0;
  }
  if (enter_blocking_section_hook != NULL) enter_blocking_section_hook();
}
/*e: function enter_blocking_section */

/*s: function leave_blocking_section */
void leave_blocking_section(void)
{
  if (leave_blocking_section_hook != NULL) leave_blocking_section_hook();
  Assert(async_signal_mode);
  async_signal_mode = 0;
}
/*e: function leave_blocking_section */

#ifndef SIGABRT
/*s: constant SIGABRT */
#define SIGABRT -1
/*e: constant SIGABRT */
#endif
#ifndef SIGALRM
/*s: constant SIGALRM */
#define SIGALRM -1
/*e: constant SIGALRM */
#endif
#ifndef SIGFPE
/*s: constant SIGFPE */
#define SIGFPE -1
/*e: constant SIGFPE */
#endif
#ifndef SIGHUP
/*s: constant SIGHUP */
#define SIGHUP -1
/*e: constant SIGHUP */
#endif
#ifndef SIGILL
/*s: constant SIGILL */
#define SIGILL -1
/*e: constant SIGILL */
#endif
#ifndef SIGINT
/*s: constant SIGINT */
#define SIGINT -1
/*e: constant SIGINT */
#endif
#ifndef SIGKILL
/*s: constant SIGKILL */
#define SIGKILL -1
/*e: constant SIGKILL */
#endif
#ifndef SIGPIPE
/*s: constant SIGPIPE */
#define SIGPIPE -1
/*e: constant SIGPIPE */
#endif
#ifndef SIGQUIT
/*s: constant SIGQUIT */
#define SIGQUIT -1
/*e: constant SIGQUIT */
#endif
#ifndef SIGSEGV
/*s: constant SIGSEGV */
#define SIGSEGV -1
/*e: constant SIGSEGV */
#endif
#ifndef SIGTERM
/*s: constant SIGTERM */
#define SIGTERM -1
/*e: constant SIGTERM */
#endif
#ifndef SIGUSR1
/*s: constant SIGUSR1 */
#define SIGUSR1 -1
/*e: constant SIGUSR1 */
#endif
#ifndef SIGUSR2
/*s: constant SIGUSR2 */
#define SIGUSR2 -1
/*e: constant SIGUSR2 */
#endif
#ifndef SIGCHLD
/*s: constant SIGCHLD */
#define SIGCHLD -1
/*e: constant SIGCHLD */
#endif
#ifndef SIGCONT
/*s: constant SIGCONT */
#define SIGCONT -1
/*e: constant SIGCONT */
#endif
#ifndef SIGSTOP
/*s: constant SIGSTOP */
#define SIGSTOP -1
/*e: constant SIGSTOP */
#endif
#ifndef SIGTSTP
/*s: constant SIGTSTP */
#define SIGTSTP -1
/*e: constant SIGTSTP */
#endif
#ifndef SIGTTIN
/*s: constant SIGTTIN */
#define SIGTTIN -1
/*e: constant SIGTTIN */
#endif
#ifndef SIGTTOU
/*s: constant SIGTTOU */
#define SIGTTOU -1
/*e: constant SIGTTOU */
#endif
#ifndef SIGVTALRM
/*s: constant SIGVTALRM */
#define SIGVTALRM -1
/*e: constant SIGVTALRM */
#endif
#ifndef SIGPROF
/*s: constant SIGPROF */
#define SIGPROF -1
/*e: constant SIGPROF */
#endif

/*s: global posix_signals */
int posix_signals[] = {
  SIGABRT, SIGALRM, SIGFPE, SIGHUP, SIGILL, SIGINT, SIGKILL, SIGPIPE,
  SIGQUIT, SIGSEGV, SIGTERM, SIGUSR1, SIGUSR2, SIGCHLD, SIGCONT,
  SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU, SIGVTALRM, SIGPROF
};
/*e: global posix_signals */

#ifndef NSIG
/*s: constant NSIG */
#define NSIG 32
/*e: constant NSIG */
#endif

/*s: function install_signal_handler */
value install_signal_handler(value signal_number, value action) /* ML */
{
  int sig;
  void (*act)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact;
#endif

  sig = Int_val(signal_number);
  if (sig < 0) sig = posix_signals[-sig-1];
  if (sig < 0 || sig >= NSIG) 
    invalid_argument("Sys.signal: unavailable signal");
  switch(action) {
  case Val_int(0):              /* Signal_default */
    act = SIG_DFL;
    break;
  case Val_int(1):              /* Signal_ignore */
    act = SIG_IGN;
    break;
  default:                      /* Signal_handle */
    if (signal_handlers == 0) {
      int i;
      Begin_root(action);
        signal_handlers = alloc_tuple(NSIG);
      End_roots();
      for (i = 0; i < NSIG; i++) Field(signal_handlers, i) = Val_int(0);
      register_global_root(&signal_handlers);
    }
    modify(&Field(signal_handlers, sig), Field(action, 0));
    act = handle_signal;
    break;
  }
#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  sigaction(sig, &sigact, NULL);
#else
  signal(sig, act);
#endif
  return Val_unit;
}
/*e: function install_signal_handler */
/*e: byterun/signals.c */
