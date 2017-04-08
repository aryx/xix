/*s: byterun/interp.c */
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

/* The bytecode interpreter */

#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "debugger.h"
#include "fail.h"
#include "fix_code.h"
#include "instrtrace.h"
#include "instruct.h"
#include "interp.h"
#include "major_gc.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "signals.h"
#include "stacks.h"
#include "str.h"

/*s: interp.c toplevel comment */
/* Registers for the abstract machine:
        pc         the code pointer
        sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment

        trapsp     pointer to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable extern_sp. */
/*e: interp.c toplevel comment */

/* Instruction decoding */

#ifdef THREADED_CODE
/*s: macro Instruct for THREADED_CODE */
#  define Instruct(name) lbl_##name
/*e: macro Instruct for THREADED_CODE */
//#  if defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#  ifdef ARCH_SIXTYFOUR
/*s: constant Jumptbl_base */
#    define Jumptbl_base ((char *) &&lbl_ACC0)
/*e: constant Jumptbl_base */
#  else
/*s: constant Jumptbl_base (byterun/interp.c) */
#    define Jumptbl_base ((char *) 0)
/*e: constant Jumptbl_base (byterun/interp.c) */
/*s: constant jumptbl_base */
#    define jumptbl_base ((char *) 0)
/*e: constant jumptbl_base */
#  endif
#  ifdef DEBUG
/*s: macro Next for THREADED_CODE ifdef DEBUG */
#    define Next goto next_instr
/*e: macro Next for THREADED_CODE ifdef DEBUG */
#  else
/*s: macro Next for THREADED_CODE */
#    define Next goto *(void *)(jumptbl_base + *pc++)
/*e: macro Next for THREADED_CODE */
#  endif
#else
/*s: macro Instruct */
#  define Instruct(name) case name
/*e: macro Instruct */
/*s: macro Next */
#  define Next break
/*e: macro Next */
#endif


/*s: constant Setup_for_gc (byterun/interp.c) */
/* GC interface */

#define Setup_for_gc { sp -= 2; sp[0] = accu; sp[1] = env; extern_sp = sp; }
/*e: constant Setup_for_gc (byterun/interp.c) */
/*s: constant Restore_after_gc (byterun/interp.c) */
#define Restore_after_gc { accu = sp[0]; env = sp[1]; sp += 2; }
/*e: constant Restore_after_gc (byterun/interp.c) */
/*s: constant Setup_for_c_call */
#define Setup_for_c_call { *--sp = env; extern_sp = sp; }
/*e: constant Setup_for_c_call */
/*s: constant Restore_after_c_call */
#define Restore_after_c_call { sp = extern_sp; env = *sp++; }
/*e: constant Restore_after_c_call */

/*s: constant Setup_for_debugger */
/* Debugger interface */

#define Setup_for_debugger \
   { sp -= 4; \
     sp[0] = accu; sp[1] = (value)(pc - 1); \
     sp[2] = env; sp[3] = Val_long(extra_args); \
     extern_sp = sp; }
/*e: constant Setup_for_debugger */
/*s: constant Restore_after_debugger */
#define Restore_after_debugger { sp += 4; }
/*e: constant Restore_after_debugger */

#ifdef THREADED_CODE
/*s: macro Restart_curr_instr if THREADED_CODE */
#define Restart_curr_instr \
  goto *(jumptable[saved_code[pc - 1 - start_code]])
/*e: macro Restart_curr_instr if THREADED_CODE */
#else
/*s: macro Restart_curr_instr */
#define Restart_curr_instr \
  curr_instr = saved_code[pc - 1 - start_code]; \
  goto dispatch_instr
/*e: macro Restart_curr_instr */
#endif

/* Register optimization.
   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

//#if defined(__GNUC__) && !defined(DEBUG)
#ifdef __GNUC__
#ifndef DEBUG
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#define ACCU_REG asm("%ebx")
#endif
#endif
#endif

/* The interpreter itself */

/*s: function interpreter */
value interprete(code_t prog, asize_t prog_size)
{
  /*s: [[interprete()]] local variables */
  /*s: [[interpreter()]] ifdef PC_REG */
  #ifdef PC_REG
    register code_t pc PC_REG;
    register value * sp SP_REG;
    register value accu ACCU_REG;
  /*e: [[interpreter()]] ifdef PC_REG */
  #else
  register code_t pc;
  register value * sp;
  register value accu;
  #endif
  /*s: [[interpreter()]] ifdef THREADED_CODE, jumptbl_base register declaration */
  //#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  #ifdef THREADED_CODE
  #ifdef ARCH_SIXTYFOUR
  #ifndef ARCH_CODE32
  #ifdef JUMPTBL_BASE_REG
    register char * jumptbl_base JUMPTBL_BASE_REG;
  #else
    register char * jumptbl_base;
  #endif
  #endif
  #endif
  #endif
  /*e: [[interpreter()]] ifdef THREADED_CODE, jumptbl_base register declaration */
  value env;
  long extra_args;

  #ifndef THREADED_CODE
  opcode_t curr_instr;
  #endif
  /*s: [[interprete()]] other local variables */
  value * modify_dest, modify_newval;
  /*x: [[interprete()]] other local variables */
  struct longjmp_buffer *    initial_external_raise;
  int                        initial_sp_offset;
  struct caml__roots_block * initial_local_roots;
  int                        initial_callback_depth;
  /*x: [[interprete()]] other local variables */
  struct longjmp_buffer raise_buf;
  /*e: [[interprete()]] other local variables */
  /*e: [[interprete()]] local variables */
  /*s: [[interprete()]] initialisations */
  /*s: [[interprete()]] ifdef THREADED_CODE initialisations */
  /*s: [[interpreter()]] ifdef THREADED_CODE, jumptable declaration */
  #ifdef THREADED_CODE
    static void * jumptable[] = {
  #    include "jumptbl.h"
    };
  #endif
  /*e: [[interpreter()]] ifdef THREADED_CODE, jumptable declaration */
  if (prog == NULL) {           /* Interpreter is initializing */
    /*s: [[interpreter()]] ifdef THREADED_CODE, initializing */
    #ifdef THREADED_CODE
        instr_table = (char **) jumptable; 
        instr_base = Jumptbl_base;
    #endif
    /*e: [[interpreter()]] ifdef THREADED_CODE, initializing */
    return Val_unit;
  }
  /*s: [[interpreter()]] ifdef THREADED_CODE, jumptbl_base setting */
  //#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  #ifdef THREADED_CODE
  #ifdef ARCH_SIXTYFOUR
  #ifndef ARCH_CODE32
    jumptbl_base = Jumptbl_base;
  #endif
  #endif
  #endif
  /*e: [[interpreter()]] ifdef THREADED_CODE, jumptbl_base setting */
  /*e: [[interprete()]] ifdef THREADED_CODE initialisations */
  /*s: [[interprete()]] initial_xxx initialisations */
  initial_local_roots = local_roots;
  initial_sp_offset = (char *) stack_high - (char *) extern_sp;
  initial_external_raise = external_raise;
  initial_callback_depth = callback_depth;
  /*e: [[interprete()]] initial_xxx initialisations */
  /*s: [[interprete()]] exception initialisations */
  if (sigsetjmp(raise_buf.buf, 1)) {
    local_roots = initial_local_roots;
    sp = extern_sp;
    callback_depth = initial_callback_depth;
    accu = exn_bucket;
    pc = NULL;
    goto raise_exception;
  }
  external_raise = &raise_buf;
  /*e: [[interprete()]] exception initialisations */

  sp = extern_sp;
  pc = prog;
  extra_args = 0;
  env = Atom(0);
  accu = Val_int(0);
  /*e: [[interprete()]] initialisations */

  /*s: [[interpreter()]] ifdef THREADED_CODE, loop start */
  #ifdef THREADED_CODE
  #ifdef DEBUG
   next_instr:
    if (icount-- == 0) stop_here ();
    Assert(sp >= stack_low);
    Assert(sp <= stack_high);
  #endif
    goto *(void *)(jumptbl_base + *pc++); /* Jump to the first instruction */
  /*e: [[interpreter()]] ifdef THREADED_CODE, loop start */
  #else
  while(1) {
    /*s: [[interpreter()]] ifdef DEBUG, loop start */
    ///disasm_instr(pc);
    #ifdef DEBUG
        if (icount-- == 0) stop_here ();
        if (trace_flag) disasm_instr(pc);
        Assert(sp >= stack_low);
        Assert(sp <= stack_high);
    #endif
    /*e: [[interpreter()]] ifdef DEBUG, loop start */
    curr_instr = *pc++;
    dispatch_instr:
    switch(curr_instr) {
  #endif /* #else THREADED_CODE */
  
    /*s: [[interpreter()]] basic stack operations cases */
    /* Basic stack operations */

        /*s: [[interpreter()]] basic stack operation before ACC case */
        Instruct(PUSHACC):
          *--sp = accu;
          /* Fallthrough */
        /*e: [[interpreter()]] basic stack operation before ACC case */
        Instruct(ACC):
          accu = sp[*pc++];
          Next;
        Instruct(PUSH): Instruct(PUSHACC0):
          *--sp = accu; 
          Next;
        Instruct(POP):
          sp += *pc++;
          Next;
        Instruct(ASSIGN):
          sp[*pc++] = accu;
          accu = Val_unit;
          Next;
    /*x: [[interpreter()]] basic stack operations cases */
        Instruct(ACC0):
          accu = sp[0]; Next;
        Instruct(ACC1):
          accu = sp[1]; Next;
        Instruct(ACC2):
          accu = sp[2]; Next;
        Instruct(ACC3):
          accu = sp[3]; Next;
        Instruct(ACC4):
          accu = sp[4]; Next;
        Instruct(ACC5):
          accu = sp[5]; Next;
        Instruct(ACC6):
          accu = sp[6]; Next;
        Instruct(ACC7):
          accu = sp[7]; Next;
    /*x: [[interpreter()]] basic stack operations cases */
        Instruct(PUSHACC1):
          *--sp = accu; accu = sp[1]; Next;
        Instruct(PUSHACC2):
          *--sp = accu; accu = sp[2]; Next;
        Instruct(PUSHACC3):
          *--sp = accu; accu = sp[3]; Next;
        Instruct(PUSHACC4):
          *--sp = accu; accu = sp[4]; Next;
        Instruct(PUSHACC5):
          *--sp = accu; accu = sp[5]; Next;
        Instruct(PUSHACC6):
          *--sp = accu; accu = sp[6]; Next;
        Instruct(PUSHACC7):
          *--sp = accu; accu = sp[7]; Next;
    /*e: [[interpreter()]] basic stack operations cases */
    /*s: [[interpreter()]] env access cases */
    /* Access in heap-allocated environment */

        /*s: [[interpreter()]] env access before ENVACC case */
        Instruct(PUSHENVACC):
          *--sp = accu;
          /* Fallthrough */
        /*e: [[interpreter()]] env access before ENVACC case */
        Instruct(ENVACC):
          accu = Field(env, *pc++);
          Next;
    /*x: [[interpreter()]] env access cases */
        Instruct(ENVACC1):
          accu = Field(env, 1); Next;
        Instruct(ENVACC2):
          accu = Field(env, 2); Next;
        Instruct(ENVACC3):
          accu = Field(env, 3); Next;
        Instruct(ENVACC4):
          accu = Field(env, 4); Next;

        Instruct(PUSHENVACC1):
          *--sp = accu; accu = Field(env, 1); Next;
        Instruct(PUSHENVACC2):
          *--sp = accu; accu = Field(env, 2); Next;
        Instruct(PUSHENVACC3):
          *--sp = accu; accu = Field(env, 3); Next;
        Instruct(PUSHENVACC4):
          *--sp = accu; accu = Field(env, 4); Next;
    /*e: [[interpreter()]] env access cases */
    /*s: [[interpreter()]] function application cases */
    /* Function application */

        Instruct(PUSH_RETADDR): {
          sp -= 3;
          sp[0] = (value) (pc + *pc);
          sp[1] = env;
          sp[2] = Val_long(extra_args);
          pc++;
          Next;
        }
        Instruct(APPLY): {
          extra_args = *pc - 1;
          pc = Code_val(accu);
          env = accu;
          goto check_stacks;
        }
        Instruct(APPTERM): {
          int nargs = *pc++;
          int slotsize = *pc;
          value * newsp;
          int i;
          /* Slide the nargs bottom words of the current frame to the top
             of the frame, and discard the remainder of the frame */
          newsp = sp + slotsize - nargs;
          for (i = nargs - 1; i >= 0; i--) newsp[i] = sp[i];
          sp = newsp;
          pc = Code_val(accu);
          env = accu;
          extra_args += nargs - 1;
          goto check_stacks;
        }
        Instruct(RETURN): {
          sp += *pc++;
          if (extra_args > 0) {
            extra_args--;
            pc = Code_val(accu);
            env = accu;
          } else {
            pc = (code_t)(sp[0]);
            env = sp[1];
            extra_args = Long_val(sp[2]);
            sp += 3;
          }
          Next;
        }
    /*x: [[interpreter()]] function application cases */
        Instruct(APPLY1): {
          value arg1 = sp[0];
          sp -= 3;
          sp[0] = arg1;
          sp[1] = (value)pc;
          sp[2] = env;
          sp[3] = Val_long(extra_args);
          pc = Code_val(accu);
          env = accu;
          extra_args = 0;
          goto check_stacks;
        }
        Instruct(APPLY2): {
          value arg1 = sp[0];
          value arg2 = sp[1];
          sp -= 3;
          sp[0] = arg1;
          sp[1] = arg2;
          sp[2] = (value)pc;
          sp[3] = env;
          sp[4] = Val_long(extra_args);
          pc = Code_val(accu);
          env = accu;
          extra_args = 1;
          goto check_stacks;
        }
        Instruct(APPLY3): {
          value arg1 = sp[0];
          value arg2 = sp[1];
          value arg3 = sp[2];
          sp -= 3;
          sp[0] = arg1;
          sp[1] = arg2;
          sp[2] = arg3;
          sp[3] = (value)pc;
          sp[4] = env;
          sp[5] = Val_long(extra_args);
          pc = Code_val(accu);
          env = accu;
          extra_args = 2;
          goto check_stacks;
        }

        Instruct(APPTERM1): {
          value arg1 = sp[0];
          sp = sp + *pc - 1;
          sp[0] = arg1;
          pc = Code_val(accu);
          env = accu;
          goto check_stacks;
        }
        Instruct(APPTERM2): {
          value arg1 = sp[0];
          value arg2 = sp[1];
          sp = sp + *pc - 2;
          sp[0] = arg1;
          sp[1] = arg2;
          pc = Code_val(accu);
          env = accu;
          extra_args += 1;
          goto check_stacks;
        }
        Instruct(APPTERM3): {
          value arg1 = sp[0];
          value arg2 = sp[1];
          value arg3 = sp[2];
          sp = sp + *pc - 3;
          sp[0] = arg1;
          sp[1] = arg2;
          sp[2] = arg3;
          pc = Code_val(accu);
          env = accu;
          extra_args += 2;
          goto check_stacks;
        }
    /*e: [[interpreter()]] function application cases */
    /*s: [[interpreter()]] misc cases */
        Instruct(RESTART): {
          int num_args = Wosize_val(env) - 2;
          int i;
          sp -= num_args;
          for (i = 0; i < num_args; i++) sp[i] = Field(env, i + 2);
          env = Field(env, 1);
          extra_args += num_args;
          Next;
        }

        Instruct(GRAB): {
          int required = *pc++;
          if (extra_args >= required) {
            extra_args -= required;
          } else {
            mlsize_t num_args, i;
            num_args = 1 + extra_args; /* arg1 + extra args */
            Alloc_small(accu, num_args + 2, Closure_tag);
            Field(accu, 1) = env;
            for (i = 0; i < num_args; i++) Field(accu, i + 2) = sp[i];
            Code_val(accu) = pc - 3; /* Point to the preceding RESTART instr. */
            sp += num_args;
            pc = (code_t)(sp[0]);
            env = sp[1];
            extra_args = Long_val(sp[2]);
            sp += 3;
          }
          Next;
        }

        Instruct(CLOSURE): {
          int nvars = *pc++;
          int i;
          if (nvars > 0) *--sp = accu;
          Alloc_small(accu, 1 + nvars, Closure_tag);
          Code_val(accu) = pc + *pc;
          for (i = 0; i < nvars; i++) Field(accu, i + 1) = sp[i];
          sp += nvars;
          pc++;
          Next;
        }

        Instruct(CLOSUREREC): {
          int nvars = *pc++;
          int i;
          if (nvars > 0) *--sp = accu;
          Alloc_small(accu, 2 + nvars, Closure_tag);
          Code_val(accu) = pc + *pc;
          Field(accu, 1) = Val_int(0);
          for (i = 0; i < nvars; i++) Field(accu, i + 2) = sp[i];
          sp += nvars;
          modify(&Field(accu, 1), accu);
          pc++;
          Next;
        }
    /*e: [[interpreter()]] misc cases */
    /*s: [[interpreter()]] global data access cases */
        /*s: [[interpreter()]] before GETGLOBAL case */
        Instruct(PUSHGETGLOBAL):
          *--sp = accu;
          /* Fallthrough */
        /*e: [[interpreter()]] before GETGLOBAL case */
        Instruct(GETGLOBAL):
          accu = Field(global_data, *pc);
          pc++;
          Next;

        Instruct(SETGLOBAL):
          modify(&Field(global_data, *pc), accu);
          accu = Val_unit;
          pc++;
          Next;
    /*x: [[interpreter()]] global data access cases */
        /*s: [[interpreter()]] before GETGLOBALFIELD case */
        Instruct(PUSHGETGLOBALFIELD):
          *--sp = accu;
          /* Fallthrough */
        /*e: [[interpreter()]] before GETGLOBALFIELD case */
        Instruct(GETGLOBALFIELD): {
          accu = Field(global_data, *pc);
          pc++;
          accu = Field(accu, *pc);
          pc++;
          Next;
        }
    /*e: [[interpreter()]] global data access cases */
    /*s: [[interpreter()]] blocks allocation cases */
    /* Allocation of blocks */
        /*s: [[interpreter()]] before ATOM case */
        Instruct(PUSHATOM):
          *--sp = accu;
          /* Fallthrough */
        /*e: [[interpreter()]] before ATOM case */
        Instruct(ATOM):
          accu = Atom(*pc++); Next;

        Instruct(MAKEBLOCK): {
          mlsize_t wosize = *pc++;
          tag_t tag = *pc++;
          mlsize_t i;
          value block;
          Alloc_small(block, wosize, tag);
          Field(block, 0) = accu;
          for (i = 1; i < wosize; i++) Field(block, i) = *sp++;
          accu = block;
          Next;
        }
    /*x: [[interpreter()]] blocks allocation cases */
        Instruct(PUSHATOM0):
          *--sp = accu;
          /* Fallthrough */
        Instruct(ATOM0):
          accu = Atom(0); Next;

        Instruct(MAKEBLOCK1): {
          tag_t tag = *pc++;
          value block;
          Alloc_small(block, 1, tag);
          Field(block, 0) = accu;
          accu = block;
          Next;
        }
        Instruct(MAKEBLOCK2): {
          tag_t tag = *pc++;
          value block;
          Alloc_small(block, 2, tag);
          Field(block, 0) = accu;
          Field(block, 1) = sp[0];
          sp += 1;
          accu = block;
          Next;
        }
        Instruct(MAKEBLOCK3): {
          tag_t tag = *pc++;
          value block;
          Alloc_small(block, 3, tag);
          Field(block, 0) = accu;
          Field(block, 1) = sp[0];
          Field(block, 2) = sp[1];
          sp += 2;
          accu = block;
          Next;
        }
    /*e: [[interpreter()]] blocks allocation cases */
    /*s: [[interpreter()]] blocks access cases */
    /* Access to components of blocks */
        Instruct(GETFIELD):
          accu = Field(accu, *pc); pc++; Next;

        Instruct(SETFIELD):
          modify_dest = &Field(accu, *pc);
          pc++;
          modify_newval = *sp++;
          goto modify;

        /*s: [[interpreter()]] before modify label */
        Instruct(SETFIELD0):
          modify_dest = &Field(accu, 0);
          modify_newval = *sp++;
        /* Fallthrough */
        /*e: [[interpreter()]] before modify label */
        modify:
          Modify(modify_dest, modify_newval);
          accu = Val_unit;
          Next;
    /*x: [[interpreter()]] blocks access cases */

        Instruct(GETFIELD0):
          accu = Field(accu, 0); Next;
        Instruct(GETFIELD1):
          accu = Field(accu, 1); Next;
        Instruct(GETFIELD2):
          accu = Field(accu, 2); Next;
        Instruct(GETFIELD3):
          accu = Field(accu, 3); Next;

        Instruct(SETFIELD1):
          modify_dest = &Field(accu, 1);
          modify_newval = *sp++;
          goto modify;
        Instruct(SETFIELD2):
          modify_dest = &Field(accu, 2);
          modify_newval = *sp++;
          goto modify;
        Instruct(SETFIELD3):
          modify_dest = &Field(accu, 3);
          modify_newval = *sp++;
          goto modify;
    /*e: [[interpreter()]] blocks access cases */
    /*s: [[interpreter()]] recursive definition cases */
    /* For recursive definitions */

        Instruct(DUMMY): {
          int size = *pc++;
          Alloc_small(accu, size, 0);
          while (size--) Field(accu, size) = Val_long(0);
          Next;
        }
        Instruct(UPDATE): {
          value newval = *sp++;
          mlsize_t size, n;
          size = Wosize_val(newval);
          Assert(size == Wosize_val(accu));
          Tag_val(accu) = Tag_val(newval);
          for (n = 0; n < size; n++) {
            modify(&Field(accu, n), Field(newval, n));
          }
          accu = Val_unit;
          Next;
        }
    /*e: [[interpreter()]] recursive definition cases */
    /*s: [[interpreter()]] branching cases */
    /* Branches and conditional branches */

        Instruct(BRANCH):
          pc += *pc;
          Next;
        Instruct(BRANCHIF):
          if (accu != Val_false) pc += *pc; else pc++;
          Next;
        Instruct(BRANCHIFNOT):
          if (accu == Val_false) pc += *pc; else pc++;
          Next;
        Instruct(SWITCH): {
          uint32 sizes = *pc++;
          if (Is_block(accu)) {
            long index = Tag_val(accu);
            Assert(index >= 0 && index < (sizes >> 16));
            pc += pc[(sizes & 0xFFFF) + index];
          } else {
            long index = Long_val(accu);
            if ((unsigned long) index < (sizes & 0xFFFF))
              pc += pc[index];
            else
              pc += (sizes & 0xFFFF) + (sizes >> 16);
          }
          Next;
        }
        Instruct(BOOLNOT):
          accu = Val_not(accu);
          Next;
    /*e: [[interpreter()]] branching cases */
    /*s: [[interpreter()]] exception cases */
    /* Exceptions */

        Instruct(PUSHTRAP):
          sp -= 4;
          Trap_pc(sp) = pc + *pc;
          Trap_link(sp) = trapsp;
          sp[2] = env;
          sp[3] = Val_long(extra_args);
          trapsp = sp;
          pc++;
          Next;

        Instruct(POPTRAP):
          /* We should check here if a signal is pending, to preserve the
             semantics of the program w.r.t. exceptions. Unfortunately,
             process_signal destroys the accumulator, and there is no
             convenient way to preserve it... */
          trapsp = Trap_link(sp);
          sp += 4;
          Next;

        Instruct(RAISE):
        raise_exception:
          if (trapsp >= trap_barrier) debugger(TRAP_BARRIER);
          if (backtrace_active) stash_backtrace(accu, pc, sp);
          sp = trapsp;
          if ((char *) sp >= (char *) stack_high - initial_sp_offset) {
            exn_bucket = accu;
            external_raise = initial_external_raise;
            extern_sp = sp;
            siglongjmp(external_raise->buf, 1);
          }
          pc = Trap_pc(sp);
          trapsp = Trap_link(sp);
          env = sp[2];
          extra_args = Long_val(sp[3]);
          sp += 4;
          Next;
    /*e: [[interpreter()]] exception cases */
    /*s: [[interpreter()]] signal cases */
    /*s: [[interpreter()]] before CHECK_SIGNALS case */
    /* Stack checks */

        check_stacks:
          if (sp < stack_threshold) {
            extern_sp = sp;
            realloc_stack();
            sp = extern_sp;
          }
          /* Fall through CHECK_SIGNALS */
    /*e: [[interpreter()]] before CHECK_SIGNALS case */

    /* Signal handling */

        Instruct(CHECK_SIGNALS):    /* accu not preserved */
          if (something_to_do) goto process_signal;
          Next;

        process_signal:
          something_to_do = 0;
          if (force_major_slice){
            Setup_for_gc;
            minor_collection ();
            Restore_after_gc;
          }
          /* If a signal arrives between the following two instructions,
             it will be lost. */
          { int signal_number = pending_signal;
            pending_signal = 0;
            if (signal_number) {
              /* Push a return frame to the current code location */
              sp -= 4;
              sp[0] = Val_int(signal_number);
              sp[1] = (value) pc;
              sp[2] = env;
              sp[3] = Val_long(extra_args);
              /* Branch to the signal handler */
              env = Field(signal_handlers, signal_number);
              pc = Code_val(env);
              extra_args = 0;
            }
          }
          Next;
    /*e: [[interpreter()]] signal cases */
    /*s: [[interpreter()]] foreign c calls cases */
    /* Calling C functions */

        Instruct(C_CALLN): {
          int nargs = *pc++;
          *--sp = accu;
          Setup_for_c_call;
          accu = cprim[*pc](sp + 1, nargs);
          Restore_after_c_call;
          sp += nargs;
          pc++;
          Next;
        }
    /*x: [[interpreter()]] foreign c calls cases */

        Instruct(C_CALL1):
          Setup_for_c_call;
          accu = cprim[*pc](accu);
          Restore_after_c_call;
          pc++;
          Next;
        Instruct(C_CALL2):
          Setup_for_c_call;
          accu = cprim[*pc](accu, sp[1]);
          Restore_after_c_call;
          sp += 1;
          pc++;
          Next;
        Instruct(C_CALL3):
          Setup_for_c_call;
          accu = cprim[*pc](accu, sp[1], sp[2]);
          Restore_after_c_call;
          sp += 2;
          pc++;
          Next;
        Instruct(C_CALL4):
          Setup_for_c_call;
          accu = cprim[*pc](accu, sp[1], sp[2], sp[3]);
          Restore_after_c_call;
          sp += 3;
          pc++;
          Next;
        Instruct(C_CALL5):
          Setup_for_c_call;
          accu = cprim[*pc](accu, sp[1], sp[2], sp[3], sp[4]);
          Restore_after_c_call;
          sp += 4;
          pc++;
          Next;
    /*e: [[interpreter()]] foreign c calls cases */
    /*s: [[interpreter()]] arithmetics cases */
    /* Integer constants */

        /*s: [[interpreter()]] before CONSTINT case */
            Instruct(PUSHCONSTINT):
              *--sp = accu;
              /* Fallthrough */
        /*e: [[interpreter()]] before CONSTINT case */
        Instruct(CONSTINT):
          accu = Val_int(*pc);
          pc++;
          Next;
    /*x: [[interpreter()]] arithmetics cases */
        Instruct(CONST0):
          accu = Val_int(0); Next;
        Instruct(CONST1):
          accu = Val_int(1); Next;
        Instruct(CONST2):
          accu = Val_int(2); Next;
        Instruct(CONST3):
          accu = Val_int(3); Next;

        Instruct(PUSHCONST0):
          *--sp = accu; accu = Val_int(0); Next;
        Instruct(PUSHCONST1):
          *--sp = accu; accu = Val_int(1); Next;
        Instruct(PUSHCONST2):
          *--sp = accu; accu = Val_int(2); Next;
        Instruct(PUSHCONST3):
          *--sp = accu; accu = Val_int(3); Next;
    /*x: [[interpreter()]] arithmetics cases */
    /* Integer arithmetic */

        Instruct(NEGINT):
          accu = (value)(2 - (long)accu); Next;
        Instruct(ADDINT):
          accu = (value)((long) accu + (long) *sp++ - 1); Next;
        Instruct(SUBINT):
          accu = (value)((long) accu - (long) *sp++ + 1); Next;
        Instruct(MULINT):
          accu = Val_long(Long_val(accu) * Long_val(*sp++)); Next;

        Instruct(DIVINT): {
          long divisor = Long_val(*sp++);
          if (divisor == 0) { Setup_for_c_call; raise_zero_divide(); }
          accu = Val_long(Long_val(accu) / divisor);
          Next;
        }
        Instruct(MODINT): {
          long divisor = Long_val(*sp++);
          if (divisor == 0) { Setup_for_c_call; raise_zero_divide(); }
          accu = Val_long(Long_val(accu) % divisor);
          Next;
        }
    /*x: [[interpreter()]] arithmetics cases */
        Instruct(ANDINT):
          accu = (value)((long) accu & (long) *sp++); Next;
        Instruct(ORINT):
          accu = (value)((long) accu | (long) *sp++); Next;
        Instruct(XORINT):
          accu = (value)(((long) accu ^ (long) *sp++) | 1); Next;
    /*x: [[interpreter()]] arithmetics cases */
        Instruct(LSLINT):
          accu = (value)((((long) accu - 1) << Long_val(*sp++)) + 1); Next;
        Instruct(LSRINT):
          accu = (value)((((unsigned long) accu - 1) >> Long_val(*sp++)) | 1);
          Next;
        Instruct(ASRINT):
          accu = (value)((((long) accu - 1) >> Long_val(*sp++)) | 1); Next;
    /*x: [[interpreter()]] arithmetics cases */
    #define Integer_comparison(opname,tst) \
        Instruct(opname): \
          accu = Val_int((long) accu tst (long) *sp++); Next;

        Integer_comparison(EQ, ==)
        Integer_comparison(NEQ, !=)
        Integer_comparison(LTINT, <)
        Integer_comparison(LEINT, <=)
        Integer_comparison(GTINT, >)
        Integer_comparison(GEINT, >=)
    /*x: [[interpreter()]] arithmetics cases */
        Instruct(OFFSETINT):
          accu += *pc << 1;
          pc++;
          Next;
        Instruct(OFFSETREF):
          Field(accu, 0) += *pc << 1;
          accu = Val_unit;
          pc++;
          Next;
    /*e: [[interpreter()]] arithmetics cases */
    /*s: [[interpreter()]] array cases */
    /* Array operations */

        Instruct(VECTLENGTH):
          accu = Val_long(Wosize_val(accu));
          Next;
        Instruct(GETVECTITEM):
          accu = Field(accu, Long_val(sp[0]));
          sp += 1;
          Next;
        Instruct(SETVECTITEM):
          modify_dest = &Field(accu, Long_val(sp[0]));
          modify_newval = sp[1];
          sp += 2;
          goto modify;
    /*e: [[interpreter()]] array cases */
    /*s: [[interpreter()]] string cases */
    /* String operations */

        Instruct(GETSTRINGCHAR):
          accu = Val_int(Byte_u(accu, Long_val(sp[0])));
          sp += 1;
          Next;
        Instruct(SETSTRINGCHAR):
          Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
          sp += 2;
          accu = Val_unit;
          Next;
    /*e: [[interpreter()]] string cases */
    /*s: [[interpreter()]] debugger cases */
    /* Debugging and machine control */

        Instruct(STOP):
          external_raise = initial_external_raise;
          extern_sp = sp;
          return accu;

        Instruct(EVENT):
          if (--event_count == 0) {
            Setup_for_debugger;
            debugger(EVENT_COUNT);
            Restore_after_debugger;
          }
          Restart_curr_instr;

        Instruct(BREAK):
          Setup_for_debugger;
          debugger(BREAKPOINT);
          Restore_after_debugger;
          Restart_curr_instr;
    /*e: [[interpreter()]] debugger cases */

    #ifndef THREADED_CODE
    default:
      fatal_error_arg("Fatal error: bad opcode (%lx)\n",
                      (char *)(long)(*(pc-1)));
    }
  }
  #endif /* ifndef THREADED_CODE */
}
/*e: function interpreter */
/*e: byterun/interp.c */
