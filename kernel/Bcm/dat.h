// pad's memory pointers (used in Lock so must be early)
// physical address
typedef uintptr phys_addr;
// virtual address (which should be a user address)
typedef uintptr virt_addr;
// kernel address (mostly physical + KZERO)
typedef uintptr kern_addr;
typedef ulong* virt_addr2;
typedef void* virt_addr3;
typedef ulong* kern_addr2;
typedef void* kern_addr3;

#include "dat_forward.h"
#include "port/portdat_forward.h"
// was a default in portdat.h
#define STAGESIZE 64 // for struct Uart

#include "dat_core.h"                    // Arch_Cpu
#include "port/portdat_core.h"        // Cpu!!, Conf, Label
#include "port/portdat_concurrency.h" // Lock
#include "dat_memory.h"                  // Arch_Proc
#include "port/portdat_memory.h"      // Page, Pagetable, Segment, KImage
#include "port/portdat_files.h"       // Chan!! Dev!!
#include "dat_time.h"                    // Arch_HZ
#include "port/portdat_time.h"        // Timer
#include "dat_processes.h"               // Arch_FPsave
#include "port/portdat_processes.h"   // Proc!!!
#include "port/portdat_console.h"     // keyboard queue, consdevtab
#include "port/portdat_devices.h"     // DevConf, DevPort (not that used)
#include "port/portdat_misc.h"        // Cmd
#include "port/portdat_buses.h"       // Uart
#include "dat_arch.h"                    // Soc

// DO NOT switch those declarations. 5c allocates R10
// for the first 'extern register' declaration seen in a file
// and R9 for the second one. See Compiler.nw.
// Then mem.h relies on this to define the UP and CPU macros used
// inside assembly code.
extern register Cpu* cpu;           /* R10 */
extern register Proc* up;           /* R9 */

// in main.c (used in mmu.c)
extern ulong memsize;

#define KMESGSIZE (16*1024) // for /dev/kmesg
