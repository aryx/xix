/*s: byterun/major_gc.h */
/*s: copyright header C damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C damien */

#ifndef _major_gc_
/*s: constant _major_gc_ */
#define _major_gc_
/*e: constant _major_gc_ */


#include "freelist.h"
#include "misc.h"

typedef struct {
  void *block;           /* address of the malloced block this chunk live in */
  asize_t alloc;         /* in bytes, used for compaction */
  asize_t size;          /* in bytes */
  char *next;
} heap_chunk_head;

/*s: function Chunk_size */
#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
/*e: function Chunk_size */
/*s: function Chunk_alloc */
#define Chunk_alloc(c) (((heap_chunk_head *) (c)) [-1]).alloc
/*e: function Chunk_alloc */
/*s: function Chunk_next */
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
/*e: function Chunk_next */
/*s: function Chunk_block */
#define Chunk_block(c) (((heap_chunk_head *) (c)) [-1]).block
/*e: function Chunk_block */

extern int gc_phase;
extern unsigned long allocated_words;
extern unsigned long extra_heap_memory;

/*s: constant Phase_mark */
#define Phase_mark 0
/*e: constant Phase_mark */
/*s: constant Phase_sweep */
#define Phase_sweep 1
/*e: constant Phase_sweep */
/*s: constant Phase_idle */
#define Phase_idle 2
/*e: constant Phase_idle */

typedef char page_table_entry;

extern char *heap_start;
extern char *heap_end;
extern unsigned long total_heap_size;
extern page_table_entry *page_table;
extern asize_t page_table_size;
extern char *gc_sweep_hp;

/*s: constant In_heap */
#define In_heap 1
/*e: constant In_heap */
/*s: constant Not_in_heap */
#define Not_in_heap 0
/*e: constant Not_in_heap */
/*s: function Page */
#define Page(p) (((addr) (p) - (addr) heap_start) >> Page_log)
/*e: function Page */
/*s: function Is_in_heap */
#define Is_in_heap(p) \
  ((addr)(p) >= (addr)heap_start && (addr)(p) < (addr)heap_end \
   && page_table [Page (p)])
/*e: function Is_in_heap */

void init_major_heap (asize_t);
asize_t round_heap_chunk_size (asize_t);
void darken (value, value *);
void major_collection_slice (void);
void major_collection (void);
void finish_major_cycle (void);


#endif /* _major_gc_ */
/*e: byterun/major_gc.h */
