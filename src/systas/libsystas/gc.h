/* tag: Tom Lord Tue Dec  4 14:41:53 2001 (gc.h)
 */

#ifndef INCLUDE__LIBSYSTAS__GC_H
#define INCLUDE__LIBSYSTAS__GC_H
/* Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */


#include "systas/libsystas/scm.h"
#include "systas/libsystas/pairs.h"



#define SCM_GCTYP16(x) 		(0xff7f & (int)SCM_CAR(x))

#define SCM_GCMARKP(x) 		(1 & (int)SCM_CDR(x))
#define SCM_GC8MARKP(x) 	(0x80 & (int)SCM_CAR(x))
#define SCM_SETGCMARK(x) 	(SCM_CDR(x) |= 1)
#define SCM_CLRGCMARK(x) 	(SCM_CDR(x) &= ~1L)
#define SCM_SETGC8MARK(x) 	(SCM_CAR(x) |= 0x80)
#define SCM_CLRGC8MARK(x) 	(SCM_CAR(x) &= ~0x80L)

#define SCM_GCCDR(x) (~1L & SCM_CDR(x))


#ifdef MDEBUG
#define SCM_NEWCELL(_into) 	scm_newcell1 (&_into)
#else
#define SCM_NEWCELL(_into) \
	do { \
	  if (SCM_IS_IMMEDIATE(scm_freelist)) \
	     _into = scm_gc_for_newcell();\
	  else \
	    { \
	       _into = scm_freelist; \
	       scm_freelist = SCM_CDR(scm_freelist);\
	       ++scm_cells_allocated; \
	    } \
	} while (0)
#endif
#define SCM_FREEP(x) (SCM_CAR(x)==scm_tc_free_cell)
#define SCM_NFREEP(x) (!SCM_FREEP(x))

extern struct scm_heap_seg_data *scm_heap_table;
extern unsigned int scm_n_heap_segs;
extern int scm_take_stdin;
extern int scm_block_gc;
extern int scm_gc_heap_lock;


extern unsigned long scm_heap_size;
extern struct scm_cell * scm_heap_org;
extern SCM scm_freelist;
extern unsigned long scm_gc_cells_collected;
extern unsigned long scm_gc_malloc_collected;
extern unsigned long scm_gc_ports_collected;
extern unsigned long scm_cells_allocated;
extern unsigned long scm_mallocated;
extern long scm_mtrigger;


/* automatically generated __STDC__ prototypes */
extern SCM scm_gc_stats (void);
extern SCM scm_object_addr (SCM obj);
extern SCM scm_gc (void);
extern void scm_gc_for_alloc (int ncells, SCM * freelistp);
extern void scm_newcell1 (SCM * into);
extern SCM scm_gc_for_newcell (void);
extern long scm_stack_size (SCM_STACKITEM *start);
extern void scm_igc (void);
extern void scm_gc_mark (SCM p);
extern void scm_mark_locations (SCM_STACKITEM x[], size_t n);
extern char * scm_must_malloc (unsigned long len);
extern char * scm_must_realloc (char *where, long olen, unsigned long len);
extern void scm_must_free (char *obj);
extern SCM scm_unhash_names (SCM names);
extern int scm_init_storage (void);
extern SCM scm_mark0 (SCM ptr);
extern SCM scm_markcdr (SCM ptr);
extern size_t scm_free0 (SCM ptr __attribute__((unused)));
extern void scm_remember (SCM * ptr);
extern SCM scm_return_first (SCM elt, ...);
extern SCM scm_permanent_object (SCM obj);
extern void scm_coextensive (SCM anchor, SCM obj);
extern void scm_init_gc (void);
#endif  /* INCLUDE__LIBSYSTAS__GC_H */
