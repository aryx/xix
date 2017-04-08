/*s: byterun/mlvalues.h */
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

#ifndef _mlvalues_
#define _mlvalues_

#include "config.h"
#include "misc.h"

/*s: mlvalues.h top comment */
/* Definitions

  word: Four bytes on 32 and 16 bit architectures,
        eight bytes on 64 bit architectures.
  long: A C long integer.
  int32: Four bytes on all architectures.

  val: The ML representation of something.  A long or a block or a pointer
       outside the heap.  If it is a block, it is the (encoded) address
       of an object.  If it is a long, it is encoded as well.

  hd: A header.
  tag: The value of the tag field of the header.
  color: The value of the color field of the header.
         This is for use only by the GC.

  block: Something allocated.  It always has a header and some
          fields or some number of bytes (a multiple of the word size).
  field: A word-sized val which is part of a block.

  hp: Pointer to the header of a block.  (a char *)
  op: Pointer to the first field of a block.  (a value *)
  bp: Pointer to the first byte of a block.  (a char *)

  Remark: A block size is always a multiple of the word size, and at least
          one word plus the header.

  bosize: Size (in bytes) of the "bytes" part.
  wosize: Size (in words) of the "fields" part.
  bhsize: Size (in bytes) of the block with its header.
  whsize: Size (in words) of the block with its header.

*/
/*e: mlvalues.h top comment */

/*s: typedef value */
typedef long value;
/*e: typedef value */
/*s: typedef header_t */
typedef unsigned long header_t;
/*e: typedef header_t */
typedef unsigned long mlsize_t;
/*s: typedef tag_t */
typedef unsigned int tag_t;             /* Actually, an unsigned char */
/*e: typedef tag_t */
/*s: typedef color_t */
typedef unsigned long color_t; // bit 8-9
/*e: typedef color_t */

typedef unsigned long mark_t;

typedef int int32;            /* Not portable, but checked by autoconf. */
typedef unsigned int uint32;  /* Seems like a reasonable assumption anyway. */

/*s: function Is_long */
/* Longs vs blocks. */
#define Is_long(x)   (((x) & 1) != 0)
/*e: function Is_long */
/*s: function Is_block */
#define Is_block(x)  (((x) & 1) == 0)
/*e: function Is_block */

/* Conversion macro names are always of the form  "to_from". */
/*s: function Val_long */
/* Example: Val_long as in "Val from long" or "Val of long". */
#define Val_long(x)     (((long)(x) << 1) + 1)
/*e: function Val_long */
/*s: function Long_val */
#define Long_val(x)     ((x) >> 1)
/*e: function Long_val */
/*s: constant Max_long */
#define Max_long ((1L << (8 * sizeof(value) - 2)) - 1)
/*e: constant Max_long */
/*s: constant Min_long */
#define Min_long (-(1L << (8 * sizeof(value) - 2)))
/*e: constant Min_long */
/*s: constant Val_int */
#define Val_int Val_long
/*e: constant Val_int */
/*s: function Int_val */
#define Int_val(x) ((int) Long_val(x))
/*e: function Int_val */

/*s: toplevel comment on header format */
/* Structure of the header:

For 16-bit and 32-bit architectures:
     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  31    10 9     8 7   0

For 64-bit architectures:

     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  63    10 9     8 7   0

*/
/*e: toplevel comment on header format */
/*s: function Tag_hd */
#define Tag_hd(hd) ((tag_t) ((hd) & 0xFF))
/*e: function Tag_hd */
/*s: function Wosize_hd */
#define Wosize_hd(hd) ((mlsize_t) ((hd) >> 10))
/*e: function Wosize_hd */

/*s: function Hd_val */
#define Hd_val(val) (((header_t *) (val)) [-1])        /* Also an l-value. */
/*e: function Hd_val */
/*s: function Hd_op */
#define Hd_op(op) (Hd_val (op))                        /* Also an l-value. */
/*e: function Hd_op */
/*s: function Hd_bp */
#define Hd_bp(bp) (Hd_val (bp))                        /* Also an l-value. */
/*e: function Hd_bp */
/*s: function Hd_hp */
#define Hd_hp(hp) (* ((header_t *) (hp)))              /* Also an l-value. */
/*e: function Hd_hp */
/*s: function Hp_val */
#define Hp_val(val) ((char *) (((header_t *) (val)) - 1))
/*e: function Hp_val */
/*s: function Hp_op */
#define Hp_op(op) (Hp_val (op))
/*e: function Hp_op */
/*s: function Hp_bp */
#define Hp_bp(bp) (Hp_val (bp))
/*e: function Hp_bp */
/*s: function Val_op */
#define Val_op(op) ((value) (op))
/*e: function Val_op */
/*s: function Val_hp */
#define Val_hp(hp) ((value) (((header_t *) (hp)) + 1))
/*e: function Val_hp */
/*s: function Op_hp */
#define Op_hp(hp) ((value *) Val_hp (hp))
/*e: function Op_hp */
/*s: function Bp_hp */
#define Bp_hp(hp) ((char *) Val_hp (hp))
/*e: function Bp_hp */

/*s: constant Num_tags */
#define Num_tags (1 << 8)
/*e: constant Num_tags */
#ifdef ARCH_SIXTYFOUR
/*s: constant Max_wosize ifdef ARCH_SIXTYFOUR */
#define Max_wosize ((1L << 54) - 1)
/*e: constant Max_wosize ifdef ARCH_SIXTYFOUR */
#else
/*s: constant Max_wosize ifndef ARCH_SIXTYFOUR */
#define Max_wosize ((1 << 22) - 1)
/*e: constant Max_wosize ifndef ARCH_SIXTYFOUR */
#endif

/*s: function Wosize_val */
#define Wosize_val(val) (Wosize_hd (Hd_val (val)))
/*e: function Wosize_val */
/*s: function Wosize_op */
#define Wosize_op(op) (Wosize_val (op))
/*e: function Wosize_op */
/*s: function Wosize_bp */
#define Wosize_bp(bp) (Wosize_val (bp))
/*e: function Wosize_bp */
/*s: function Wosize_hp */
#define Wosize_hp(hp) (Wosize_hd (Hd_hp (hp)))
/*e: function Wosize_hp */
/*s: function Whsize_wosize */
#define Whsize_wosize(sz) ((sz) + 1)
/*e: function Whsize_wosize */
/*s: function Wosize_whsize */
#define Wosize_whsize(sz) ((sz) - 1)
/*e: function Wosize_whsize */
/*s: function Wosize_bhsize */
#define Wosize_bhsize(sz) ((sz) / sizeof (value) - 1)
/*e: function Wosize_bhsize */
/*s: function Bsize_wsize */
#define Bsize_wsize(sz) ((sz) * sizeof (value))
/*e: function Bsize_wsize */
/*s: function Wsize_bsize */
#define Wsize_bsize(sz) ((sz) / sizeof (value))
/*e: function Wsize_bsize */
/*s: function Bhsize_wosize */
#define Bhsize_wosize(sz) (Bsize_wsize (Whsize_wosize (sz)))
/*e: function Bhsize_wosize */
/*s: function Bhsize_bosize */
#define Bhsize_bosize(sz) ((sz) + sizeof (header_t))
/*e: function Bhsize_bosize */
/*s: function Bosize_val */
#define Bosize_val(val) (Bsize_wsize (Wosize_val (val)))
/*e: function Bosize_val */
/*s: function Bosize_op */
#define Bosize_op(op) (Bosize_val (Val_op (op)))
/*e: function Bosize_op */
/*s: function Bosize_bp */
#define Bosize_bp(bp) (Bosize_val (Val_bp (bp)))
/*e: function Bosize_bp */
/*s: function Bosize_hd */
#define Bosize_hd(hd) (Bsize_wsize (Wosize_hd (hd)))
/*e: function Bosize_hd */
/*s: function Whsize_hp */
#define Whsize_hp(hp) (Whsize_wosize (Wosize_hp (hp)))
/*e: function Whsize_hp */
/*s: function Whsize_val */
#define Whsize_val(val) (Whsize_hp (Hp_val (val)))
/*e: function Whsize_val */
/*s: function Whsize_bp */
#define Whsize_bp(bp) (Whsize_val (Val_bp (bp)))
/*e: function Whsize_bp */
/*s: function Whsize_hd */
#define Whsize_hd(hd) (Whsize_wosize (Wosize_hd (hd)))
/*e: function Whsize_hd */
/*s: function Bhsize_hp */
#define Bhsize_hp(hp) (Bsize_wsize (Whsize_hp (hp)))
/*e: function Bhsize_hp */
/*s: function Bhsize_hd */
#define Bhsize_hd(hd) (Bsize_wsize (Whsize_hd (hd)))
/*e: function Bhsize_hd */

#ifdef ARCH_BIG_ENDIAN
/*s: function Tag_val ifdef ARCH_BIG_ENDIAN */
#define Tag_val(val) (((unsigned char *) (val)) [-1])
/*e: function Tag_val ifdef ARCH_BIG_ENDIAN */
                                                 /* Also an l-value. */
/*s: function Tag_hp ifdef ARCH_BIG_ENDIAN */
#define Tag_hp(hp) (((unsigned char *) (hp)) [sizeof(value)-1])
/*e: function Tag_hp ifdef ARCH_BIG_ENDIAN */
                                                 /* Also an l-value. */
#else
/*s: function Tag_val little endian */
#define Tag_val(val) (((unsigned char *) (val)) [-sizeof(value)])
/*e: function Tag_val little endian */
                                                 /* Also an l-value. */
/*s: function Tag_hp little endian */
#define Tag_hp(hp) (((unsigned char *) (hp)) [0])
/*e: function Tag_hp little endian */
                                                 /* Also an l-value. */
#endif

/*s: constant No_scan_tag */
/* The lowest tag for blocks containing no value. */
#define No_scan_tag 251
/*e: constant No_scan_tag */


/* 1- If tag < No_scan_tag : a tuple of fields.  */

/*s: function Op_val */
/* Pointer to the first field. */
#define Op_val(x) ((value *) (x))
/*e: function Op_val */
/*s: function Field */
/* Fields are numbered from 0. */
#define Field(x, i) (((value *)(x)) [i])           /* Also an l-value. */
/*e: function Field */

/*s: typedef opcode_t */
typedef int32 opcode_t;
/*e: typedef opcode_t */
/*s: typedef code_t */
typedef opcode_t * code_t;
/*e: typedef code_t */

/*s: constant Closure_tag */
/* Special case of tuples of fields: closures */

#define Closure_tag 250
/*e: constant Closure_tag */
/*s: function Code_val */
#define Code_val(val) (((code_t *) (val)) [0])     /* Also an l-value. */
/*e: function Code_val */

/* If tag == Infix_tag : an infix header inside a closure */
/* Infix_tag must be odd so that the infix header is scanned as an integer */
/*s: constant Infix_tag */
/* Infix_tag must be 1 modulo 4 and infix headers can only occur in blocks
   with tag Closure_tag (see compact.c). */

#define Infix_tag 249
/*e: constant Infix_tag */
/*s: function Infix_offset_hd */
#define Infix_offset_hd(hd) (Bosize_hd(hd))
/*e: function Infix_offset_hd */
/*s: function Infix_offset_val */
#define Infix_offset_val(v) Infix_offset_hd(Hd_val(v))
/*e: function Infix_offset_val */

/*s: constant Object_tag */
/* Another special case: objects */
#define Object_tag 248
/*e: constant Object_tag */
/*s: function Class_val */
#define Class_val(val) Field(val, 0)
/*e: function Class_val */
/*s: function Oid_val */
#define Oid_val(val) Long_val(Field(val, 1))
/*e: function Oid_val */

/* 2- If tag > No_scan_tag : a sequence of bytes. */

/*s: function Bp_val */
/* Pointer to the first byte */
#define Bp_val(v) ((char *) (v))
/*e: function Bp_val */
/*s: function Val_bp */
#define Val_bp(p) ((value) (p))
/*e: function Val_bp */
/*s: function Byte */
/* Bytes are numbered from 0. */
#define Byte(x, i) (((char *) (x)) [i])            /* Also an l-value. */
/*e: function Byte */
/*s: function Byte_u */
#define Byte_u(x, i) (((unsigned char *) (x)) [i]) /* Also an l-value. */
/*e: function Byte_u */

/*s: constant Abstract_tag */
/* Abstract things.  Their contents is not traced by the GC; therefore they
   must not contain any [value].
*/
#define Abstract_tag 251
/*e: constant Abstract_tag */

/*s: constant String_tag */
/* Strings. */
#define String_tag 252
/*e: constant String_tag */
/*s: function String_val */
#define String_val(x) ((char *) Bp_val(x))
/*e: function String_val */
extern mlsize_t string_length (value v);   /* size in bytes */

/*s: constant Double_tag */
/* Floating-point numbers. */
#define Double_tag 253
/*e: constant Double_tag */
/*s: constant Double_wosize */
#define Double_wosize ((sizeof(double) / sizeof(value)))
/*e: constant Double_wosize */

#ifndef ARCH_ALIGN_DOUBLE
/*s: macro Double_val */
#define Double_val(v) (* (double *)(v))
/*e: macro Double_val */
/*s: macro Store_double_val */
#define Store_double_val(v,d) (* (double *)(v) = (d))
/*e: macro Store_double_val */
#else
double Double_val (value);
void Store_double_val (value,double);
#endif

/*s: constant Double_array_tag */
/* Arrays of floating-point numbers. */
#define Double_array_tag 254
/*e: constant Double_array_tag */
/*s: function Double_field */
#define Double_field(v,i) Double_val((value)((double *)(v) + (i)))
/*e: function Double_field */
/*s: function Store_double_field */
#define Store_double_field(v,i,d) \
  Store_double_val((value)((double *)(v) + (i)),d)
/*e: function Store_double_field */

/*s: constant Final_tag */
/* Finalized things.  Just like abstract things, but the GC will call the
   [Final_fun] before deallocation.
*/
#define Final_tag 255
/*e: constant Final_tag */
typedef void (*final_fun) (value);
/*s: function Final_fun */
#define Final_fun(val) (((final_fun *) (val)) [0]) /* Also an l-value. */
/*e: function Final_fun */


/* 3- Atoms are 0-tuples.  They are statically allocated once and for all. */

extern header_t atom_table[];
/*s: function Atom */
#define Atom(tag) (Val_hp (&(atom_table [tag])))
/*e: function Atom */

/* Is_atom tests whether a well-formed block is statically allocated
   outside the heap. For the bytecode system, only zero-sized block (Atoms)
   fall in this class. For the native-code generator, data
   emitted by the code generator (as described in the table
   caml_data_segments) are also atoms. */

#ifndef NATIVE_CODE
/*s: function Is_atom */
#define Is_atom(v) ((v) >= Atom(0) && (v) <= Atom(255))
/*e: function Is_atom */
#else
extern char * static_data_start, * static_data_end;
/*s: function Is_atom ifdef NATIVE_CODE */
#define Is_atom(v) \
  ((((char *)(v) >= static_data_start && (char *)(v) < static_data_end) || \
   ((v) >= Atom(0) && (v) <= Atom(255))))
/*e: function Is_atom ifdef NATIVE_CODE */
#endif

/*s: function Val_bool */
/* Booleans are integers 0 or 1 */

#define Val_bool(x) Val_int((x) != 0)
/*e: function Val_bool */
/*s: function Bool_val */
#define Bool_val(x) Int_val(x)
/*e: function Bool_val */
/*s: constant Val_false */
#define Val_false Val_int(0)
/*e: constant Val_false */
/*s: constant Val_true */
#define Val_true Val_int(1)
/*e: constant Val_true */
/*s: function Val_not */
#define Val_not(x) (4 - (x))
/*e: function Val_not */

/*s: constant Val_unit */
/* The unit value is 0 */

#define Val_unit Val_int(0)
/*e: constant Val_unit */

/* The table of global identifiers */

extern value global_data;


#endif /* _mlvalues_ */
/*e: byterun/mlvalues.h */
