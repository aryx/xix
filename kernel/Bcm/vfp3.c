/*
 * VFPv2 or VFPv3 floating point unit
 */
/* only called to deal with user-mode instruction faults */
#include <u.h>
#include "../port/lib.h"
#include "../port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "ureg.h"
#include "arm.h"

enum VFPKind {
    VFPv2   = 2,
    VFPv3   = 3,
};

enum {
    Fpsid = 0,
    Fpscr = 1,          /* rw */
    Mvfr1 = 6,
    Mvfr0 = 7,
    Fpexc = 8,          /* rw */
    Fpinst= 9,          /* optional, for exceptions */
    Fpinst2=10,
};

enum {
    /* Fpexc bits */
    Fpex =      1u << 31,
    Fpenabled = 1 << 30,
    Fpdex =     1 << 29,    /* defined synch exception */
    /* FSR bits appear here */
    Fpmbc =     Fpdex,      /* bits exception handler must clear */

    /* Fpscr bits; see u.h for more */
    Stride =    MASK(2) << 20,
    Len =       MASK(3) << 16,
    Dn=     1 << 25,
    Fz=     1 << 24,
    /* trap exception enables (not allowed in vfp3) */
    FPIDNRM =   1 << 15,    /* input denormal */
    Alltraps = FPIDNRM | FPINEX | FPUNFL | FPOVFL | FPZDIV | FPINVAL,
    /* pending exceptions */
    FPAIDNRM =  1 << 7,     /* input denormal */
    Allexc = FPAIDNRM | FPAINEX | FPAUNFL | FPAOVFL | FPAZDIV | FPAINVAL,
    /* condition codes */
    Allcc =     MASK(4) << 28,
};

enum {
    /* CpCPaccess bits */
    Cpaccnosimd =   1u << 31,
    Cpaccd16 =  1 << 30,
};

static char *
subarch(int impl, uint sa)
{
    static char *armarchs[] = {
        "VFPv1 (unsupported)",
        "VFPv2",
        "VFPv3+ with common VFP subarch v2",
        "VFPv3+ with null subarch",
        "VFPv3+ with common VFP subarch v3",
    };

    if (impl != 'A' || sa >= nelem(armarchs))
        return "GOK";
    else
        return armarchs[sa];
}

static char *
implement(uchar impl)
{
    if (impl == 'A')
        return "arm";
    else
        return "unknown";
}

static int
havefp(void)
{
    int gotfp;
    ulong acc, sid;

    if (cpu->havefpvalid)
        return cpu->havefp;

    cpu->havefp = 0;
    gotfp = 1 << CpFP | 1 << CpDFP;
    cpwrsc(0, CpCONTROL, 0, CpCPaccess, MASK(28));
    acc = cprdsc(0, CpCONTROL, 0, CpCPaccess);
    if ((acc & (MASK(2) << (2*CpFP))) == 0) {
        gotfp &= ~(1 << CpFP);
        print("fpon: no single FP coprocessor\n");
    }
    if ((acc & (MASK(2) << (2*CpDFP))) == 0) {
        gotfp &= ~(1 << CpDFP);
        print("fpon: no double FP coprocessor\n");
    }
    if (!gotfp) {
        print("fpon: no FP coprocessors\n");
        cpu->havefpvalid = true;
        return 0;
    }
    cpu->fpon = true;          /* don't panic */
    sid = fprd(Fpsid);
    cpu->fpon = false;

    switch((sid >> 16) & MASK(7)){
    case 0:             /* VFPv1 */
        break;
    case 1:             /* VFPv2 */
        cpu->havefp = VFPv2;
        cpu->fpnregs = 16;
        break;
    default:            /* VFPv3 or later */
        cpu->havefp = VFPv3;
        cpu->fpnregs = (acc & Cpaccd16) ? 16 : 32;
        break;
    }
    if (cpu->cpuno == 0)
        print("fp: %d registers, %s simd\n", cpu->fpnregs,
            (acc & Cpaccnosimd? " no": ""));
    cpu->havefpvalid = true;
    return 1;
}

/*
 * these can be called to turn the fpu on or off for user procs,
 * not just at system start up or shutdown.
 */

void
fpoff(void)
{
    if (cpu->fpon) {
        fpwr(Fpexc, 0);
        cpu->fpon = false;
    }
}

void
fpononly(void)
{
    if (!cpu->fpon && havefp()) {
        /* enable fp.  must be first operation on the FPUs. */
        fpwr(Fpexc, Fpenabled);
        cpu->fpon = true;
    }
}

static void
fpcfg(void)
{
    int impl;
    ulong sid;
    static int printed;

    /* clear pending exceptions; no traps in vfp3; all v7 ops are scalar */
    cpu->fpscr = Dn | Fz | FPRNR | (FPINVAL | FPZDIV | FPOVFL) & ~Alltraps;
    fpwr(Fpscr, cpu->fpscr);
    cpu->fpconfiged = true;

    if (printed)
        return;
    sid = fprd(Fpsid);
    impl = sid >> 24;
    print("fp: %s arch %s; rev %ld\n", implement(impl),
        subarch(impl, (sid >> 16) & MASK(7)), sid & MASK(4));
    printed = 1;
}

void
fpinit(void)
{
    if (havefp()) {
        fpononly();
        fpcfg();
    }
}

void
fpon(void)
{
    if (havefp()) {
      fpononly();
        if (cpu->fpconfiged)
            fpwr(Fpscr, (fprd(Fpscr) & Allcc) | cpu->fpscr);
        else
            fpcfg();    /* 1st time on this fpu; configure it */
    }
}

void
fpclear(void)
{
//  ulong scr;

    fpon();
//  scr = fprd(Fpscr);
//  cpu->fpscr = scr & ~Allexc;
//  fpwr(Fpscr, cpu->fpscr);

    fpwr(Fpexc, fprd(Fpexc) & ~Fpmbc);
}

static void
fprestore(Proc *p)
{
    int n;

    fpon();
    fpwr(Fpscr, p->fpsave.control);
    cpu->fpscr = fprd(Fpscr) & ~Allcc;
    assert(cpu->fpnregs);
    for (n = 0; n < cpu->fpnregs; n++)
        fprestreg(n, *(uvlong *)p->fpsave.regs[n]);
}


static void
mathnote(void)
{
    ulong status;
    char *msg, note[ERRMAX];

    status = up->fpsave.status;

    /*
     * Some attention should probably be paid here to the
     * exception masks and error summary.
     */
    if (status & FPAINEX)
        msg = "inexact";
    else if (status & FPAOVFL)
        msg = "overflow";
    else if (status & FPAUNFL)
        msg = "underflow";
    else if (status & FPAZDIV)
        msg = "divide by zero";
    else if (status & FPAINVAL)
        msg = "bad operation";
    else
        msg = "spurious";
    snprint(note, sizeof note, "sys: fp: %s fppc=%#p status=%#lux",
        msg, up->fpsave.pc, status);
    postnote(up, 1, note, NDebug);
}


static void
mathemu(Ureg *)
{
    switch(up->fpstate){
    case FPinit:
        fpinit();
        up->fpstate = FPactive;
        break;
    case FPinactive:
        /*
         * Before restoring the state, check for any pending
         * exceptions.  There's no way to restore the state without
         * generating an unmasked exception.
         * More attention should probably be paid here to the
         * exception masks and error summary.
         */
        if(up->fpsave.status & (FPAINEX|FPAUNFL|FPAOVFL|FPAZDIV|FPAINVAL)){
            mathnote();
            break;
        }
        fprestore(up);
        up->fpstate = FPactive;
        break;
    case FPactive:
        error("illegal instruction: bad vfp fpu opcode");
        break;
    }
    fpclear();
}


void
fpstuck(uintptr pc)
{
    if (cpu->fppc == pc && cpu->fppid == up->pid) {
        cpu->fpcnt++;
        if (cpu->fpcnt > 4)
            panic("fpuemu: cpu%d stuck at pid %ld %s pc %#p "
                "instr %#8.8lux", cpu->cpuno, up->pid, up->text,
                pc, *(ulong *)pc);
    } else {
        cpu->fppid = up->pid;
        cpu->fppc = pc;
        cpu->fpcnt = 0;
    }
}

enum {
    N = 1<<31,
    Z = 1<<30,
    C = 1<<29,
    V = 1<<28,
    REGPC = 15,
};


static int
condok(int cc, int c)
{
    switch(c){
    case 0: /* Z set */
        return cc&Z;
    case 1: /* Z clear */
        return (cc&Z) == 0;
    case 2: /* C set */
        return cc&C;
    case 3: /* C clear */
        return (cc&C) == 0;
    case 4: /* N set */
        return cc&N;
    case 5: /* N clear */
        return (cc&N) == 0;
    case 6: /* V set */
        return cc&V;
    case 7: /* V clear */
        return (cc&V) == 0;
    case 8: /* C set and Z clear */
        return cc&C && (cc&Z) == 0;
    case 9: /* C clear or Z set */
        return (cc&C) == 0 || cc&Z;
    case 10:    /* N set and V set, or N clear and V clear */
        return (~cc&(N|V))==0 || (cc&(N|V)) == 0;
    case 11:    /* N set and V clear, or N clear and V set */
        return (cc&(N|V))==N || (cc&(N|V))==V;
    case 12:    /* Z clear, and either N set and V set or N clear and V clear */
        return (cc&Z) == 0 && ((~cc&(N|V))==0 || (cc&(N|V))==0);
    case 13:    /* Z set, or N set and V clear or N clear and V set */
        return (cc&Z) || (cc&(N|V))==N || (cc&(N|V))==V;
    case 14:    /* always */
        return 1;
    case 15:    /* never (reserved) */
        return 0;
    }
    return 0;   /* not reached */
}

#define CpOFPA      1           /* ancient 7500 FPA */

#define ISCPOP(op)  ((op) == 0xE || ((op) & ~1) == 0xC)
#define ISVFPOP(cp, op) (((cp) == CpDFP || (cp) == CpFP) && ISCPOP(op))
#define ISFPAOP(cp, op) ((cp) == CpOFPA && ISCPOP(op))


int
fpuemu(Ureg* ureg)
{
    int s, nfp, cop, op;
    uintptr pc;

    //TODO: if(waserror()){
    //    postnote(up, 1, up->errstr, NDebug);
    //    return 1;
    //}

    //if(up->fpstate & FPillegal)
    //    error("floating point in note handler");
    print("fpemu\n");

    nfp = 0;
    pc = ureg->pc;
    //TODO: validaddr(pc, 4, 0);
    if(!condok(ureg->psr, *(ulong*)pc >> 28))
        iprint("fpuemu: conditional instr shouldn't have got here\n");
    op  = (*(ulong *)pc >> 24) & MASK(4);
    cop = (*(ulong *)pc >>  8) & MASK(4);

    iprint("fpuemu: cop = %d\n", cop);

    if(cpu->fpon)
        fpstuck(pc);        /* debugging; could move down 1 line */
    if (ISVFPOP(cop, op)) {  /* if vfp, fpu must be off */
        mathemu(ureg);      /* enable fpu & retry */
        nfp = 1;
    }
    if (ISFPAOP(cop, op)) {     /* old arm 7500 fpa opcode? */
      iprint("fpuemu: fpa instr %#8.8lux at %#p\n", *(ulong *)pc, pc);
      panic("illegal instruction: old arm 7500 fpa opcode");
    }

    //TODO: poperror();
    return nfp;
}
