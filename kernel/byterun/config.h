/*s: byterun/config.h */
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

#ifndef _config_
#define _config_

#include "config/m.h"
#include "config/s.h"

#include "plan9.h"

/* Library dependencies */

/*s: function bcopy */
//#define bcopy(src,dst,len) memmove((dst), (src), (len))
/*e: function bcopy */


/* We use threaded code interpretation if the compiler provides labels
   as first-class values (GCC 2.x).
   Macintosh 68k also uses threaded code, with the assembly-language
   bytecode interpreter (THREADED_CODE defined in config/sm-Mac.h).
*/
//#if defined(__GNUC__) && __GNUC__ >= 2 && !defined(DEBUG)
#ifdef __GNUC__
/*s: constant THREADED_CODE */
#define THREADED_CODE
/*e: constant THREADED_CODE */
#endif

/* Signed char type */

typedef signed char schar;

/*s: constant Page_size */
/* Do not change this definition. */
#define Page_size (1 << Page_log)
/*e: constant Page_size */

/* Memory model parameters */

/*s: constant Page_log */
/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */
/*e: constant Page_log */

/*s: constant Stack_size */
/* Initial size of stack (bytes). */
#define Stack_size (4096 * sizeof(value))
/*e: constant Stack_size */

/*s: constant Stack_threshold */
/* Minimum free size of stack (bytes); below that, it is reallocated. */
#define Stack_threshold (256 * sizeof(value))
/*e: constant Stack_threshold */

/*s: constant Max_stack_def */
/* Default maximum size of the stack (words). */
#define Max_stack_def (256 * 1024)
/*e: constant Max_stack_def */


/*s: constant Max_young_wosize */
/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256
/*e: constant Max_young_wosize */


/*s: constant Minor_heap_min */
/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096
/*e: constant Minor_heap_min */

/*s: constant Minor_heap_max */
/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)
/*e: constant Minor_heap_max */

/*s: constant Minor_heap_def */
/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768
/*e: constant Minor_heap_def */


/*s: constant Heap_chunk_min */
/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))
/*e: constant Heap_chunk_min */

/*s: constant Heap_chunk_max */
/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))
/*e: constant Heap_chunk_max */

/*s: constant Heap_chunk_def */
/* Default size increment when growing the heap. (words)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * 1024)
/*e: constant Heap_chunk_def */

/*s: constant Init_heap_def */
/* Default initial size of the major heap (words);
   same constraints as for Heap_chunk_def. */
#define Init_heap_def (62 * 1024)
/*e: constant Init_heap_def */


/*s: constant Percent_free_def */
/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
#define Percent_free_def 42
/*e: constant Percent_free_def */

/*s: constant Max_percent_free_def */
/* Default setting for the compacter: off */
#define Max_percent_free_def 1000000
/*e: constant Max_percent_free_def */


#endif /* _config_ */
/*e: byterun/config.h */
