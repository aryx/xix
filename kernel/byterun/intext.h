/*s: byterun/intext.h */
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

/* Structured input/output */

#ifndef _intext_
#define _intext_

#include "misc.h"
#include "mlvalues.h"
#include "io.h"

/*s: constant Intext_magic_number */
/* Magic number */

#define Intext_magic_number 0x8495A6BE
/*e: constant Intext_magic_number */

/*s: constant PREFIX_SMALL_BLOCK */
/* Codes for the compact format */

#define PREFIX_SMALL_BLOCK 0x80
/*e: constant PREFIX_SMALL_BLOCK */
/*s: constant PREFIX_SMALL_INT */
#define PREFIX_SMALL_INT 0x40
/*e: constant PREFIX_SMALL_INT */
/*s: constant PREFIX_SMALL_STRING */
#define PREFIX_SMALL_STRING 0x20
/*e: constant PREFIX_SMALL_STRING */
/*s: constant CODE_INT8 */
#define CODE_INT8 0x0
/*e: constant CODE_INT8 */
/*s: constant CODE_INT16 */
#define CODE_INT16 0x1
/*e: constant CODE_INT16 */
/*s: constant CODE_INT32 */
#define CODE_INT32 0x2
/*e: constant CODE_INT32 */
/*s: constant CODE_INT64 */
#define CODE_INT64 0x3
/*e: constant CODE_INT64 */
/*s: constant CODE_SHARED8 */
#define CODE_SHARED8 0x4
/*e: constant CODE_SHARED8 */
/*s: constant CODE_SHARED16 */
#define CODE_SHARED16 0x5
/*e: constant CODE_SHARED16 */
/*s: constant CODE_SHARED32 */
#define CODE_SHARED32 0x6
/*e: constant CODE_SHARED32 */
/*s: constant CODE_BLOCK32 */
#define CODE_BLOCK32 0x8
/*e: constant CODE_BLOCK32 */
/*s: constant CODE_STRING8 */
#define CODE_STRING8 0x9
/*e: constant CODE_STRING8 */
/*s: constant CODE_STRING32 */
#define CODE_STRING32 0xA
/*e: constant CODE_STRING32 */
/*s: constant CODE_DOUBLE_BIG */
#define CODE_DOUBLE_BIG 0xB
/*e: constant CODE_DOUBLE_BIG */
/*s: constant CODE_DOUBLE_LITTLE */
#define CODE_DOUBLE_LITTLE 0xC
/*e: constant CODE_DOUBLE_LITTLE */
/*s: constant CODE_DOUBLE_ARRAY8_BIG */
#define CODE_DOUBLE_ARRAY8_BIG 0xD
/*e: constant CODE_DOUBLE_ARRAY8_BIG */
/*s: constant CODE_DOUBLE_ARRAY8_LITTLE */
#define CODE_DOUBLE_ARRAY8_LITTLE 0xE
/*e: constant CODE_DOUBLE_ARRAY8_LITTLE */
/*s: constant CODE_DOUBLE_ARRAY32_BIG */
#define CODE_DOUBLE_ARRAY32_BIG 0xF
/*e: constant CODE_DOUBLE_ARRAY32_BIG */
/*s: constant CODE_DOUBLE_ARRAY32_LITTLE */
#define CODE_DOUBLE_ARRAY32_LITTLE 0x7
/*e: constant CODE_DOUBLE_ARRAY32_LITTLE */
/*s: constant CODE_CODEPOINTER */
#define CODE_CODEPOINTER 0x10
/*e: constant CODE_CODEPOINTER */
/*s: constant CODE_INFIXPOINTER */
#define CODE_INFIXPOINTER 0x11
/*e: constant CODE_INFIXPOINTER */

#ifdef ARCH_BIG_ENDIAN
/*s: constant CODE_DOUBLE_NATIVE */
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_BIG
/*e: constant CODE_DOUBLE_NATIVE */
/*s: constant CODE_DOUBLE_ARRAY8_NATIVE */
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_BIG
/*e: constant CODE_DOUBLE_ARRAY8_NATIVE */
/*s: constant CODE_DOUBLE_ARRAY32_NATIVE */
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_BIG
/*e: constant CODE_DOUBLE_ARRAY32_NATIVE */
#else
/*s: constant CODE_DOUBLE_NATIVE (byterun/intext.h) */
#define CODE_DOUBLE_NATIVE CODE_DOUBLE_LITTLE
/*e: constant CODE_DOUBLE_NATIVE (byterun/intext.h) */
/*s: constant CODE_DOUBLE_ARRAY8_NATIVE (byterun/intext.h) */
#define CODE_DOUBLE_ARRAY8_NATIVE CODE_DOUBLE_ARRAY8_LITTLE
/*e: constant CODE_DOUBLE_ARRAY8_NATIVE (byterun/intext.h) */
/*s: constant CODE_DOUBLE_ARRAY32_NATIVE (byterun/intext.h) */
#define CODE_DOUBLE_ARRAY32_NATIVE CODE_DOUBLE_ARRAY32_LITTLE
/*e: constant CODE_DOUBLE_ARRAY32_NATIVE (byterun/intext.h) */
#endif

/* Initial sizes of data structures for extern */

#ifndef INITIAL_EXTERN_BLOCK_SIZE
/*s: constant INITIAL_EXTERN_BLOCK_SIZE */
#define INITIAL_EXTERN_BLOCK_SIZE 8192
/*e: constant INITIAL_EXTERN_BLOCK_SIZE */
#endif

#ifndef INITIAL_EXTERN_TABLE_SIZE
/*s: constant INITIAL_EXTERN_TABLE_SIZE */
#define INITIAL_EXTERN_TABLE_SIZE 2039
/*e: constant INITIAL_EXTERN_TABLE_SIZE */
#endif

/*s: constant INITIAL_OFFSET_MAX */
/* Maximal value of initial_ofs above which we should start again with
   initial_ofs = 1. Should be low enough to prevent rollover of initial_ofs
   next time we extern a structure. Since a structure contains at most 
   2^N / (2 * sizeof(value)) heap objects (N = 32 or 64 depending on target),
   any value below 2^N - (2^N / (2 * sizeof(value))) suffices.
   We just take 2^(N-1) for simplicity. */

#define INITIAL_OFFSET_MAX ((unsigned long)1 << (8 * sizeof(value) - 1))
/*e: constant INITIAL_OFFSET_MAX */

/* The entry points */

void output_val (struct channel * chan, value v, value flags);
value input_val (struct channel * chan);
value input_val_from_string (value str, long ofs);

/* Auxiliary stuff for sending code pointers */
unsigned char * code_checksum (void);

#ifndef NATIVE_CODE
#include "fix_code.h"
/*s: constant code_area_start */
#define code_area_start ((char *) start_code)
/*e: constant code_area_start */
/*s: constant code_area_end */
#define code_area_end ((char *) start_code + code_size)
/*e: constant code_area_end */
#else
extern char * code_area_start, * code_area_end;
#endif


#endif

/*e: byterun/intext.h */
