
/*
 * fasttick timer interrupts
 */
enum Timermode 
{
    Trelative,  /* timer programmed in ns from now */
    Tperiodic,  /* periodic timer, period in ns */
};

typedef vlong   Tval; // ticks
typedef vlong   Tnano; // nanoseconds
typedef vlong   Tmicro; // microseconds
typedef int     Tms; // milliseconds
typedef vlong   Tsec; // seconds

struct Timer
{
    /* Public interface */
    // enum<timermode>
    int tmode;    /* See above */
    Tnano tns;    /* meaning defined by mode */ //nanosecond
    void  (*tf)(Ureg*, Timer*);
    void  *ta;
  
    /* Internal */
    Lock;
    Tval  tticks;   /* tns converted to ticks */
    Tval  twhen;    /* ns represented in arch_fastticks */

    // list<Timer> of Timers.head
    Timer *tnext;
    // ref<list<Timer>> Timers.head
    Timers  *tt;    /* Timers queue this timer runs on */
    };

// was in clock.c
struct Timers
{
    // list<Timer> (next = Timer.tnext)
    Timer *head;
    // extra
    Lock;
};

// used only in arm/ for now
enum {
 Mhz	= 1000 * 1000,
};

// <arch>/dat_time.h should defined Arch_HZ

#pragma varargck  type  "t"   long
#pragma varargck  type  "U"   uvlong
