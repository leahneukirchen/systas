/* gc.c - garbage collection
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */


#include <unistd.h>
#include <stdlib.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "systas/libsystas/gc.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/stime.h"
#include "systas/libsystas/async.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/weaks.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/stackchk.h"
#include "systas/libsystas/struct.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/continuations.h"
#include "systas/libsystas/hashtab.h"

/************************************************************************
 *(h0 "Garbage Collection")
 * 
 * The garbage collector in Systas Scheme is a simple mark-sweeep collector
 * that conservatively scans the C stack for GC roots.
 * 
 * \Note:/ Conservative stack scanning is highly problematic for applications
 * requiring rock-solid reliability and performance.  Some future release
 * of Systas will use a different GC strategy.
 * 
 */



static void scm_alloc_some_heap (int ncells, SCM * freelistp);
static void scm_mark_weak_vector_spines (void);
static void scm_gc_sweep (void);



SCM_SYMBOL (s_heap, "heap");
SCM_SYMBOL (s_gc_mark, "gc_mark");
SCM_SYMBOL (s_gc_sweep, "gc_sweep");
SCM_SYMBOL (s_hplims, "hplims");


/* {heap tuning parameters}
 * 
 * These are parameters for controlling memory allocation.  The heap
 * is the area out of which scm_cons, and object headers are allocated.
 *
 * Each heap cell is 8 bytes on a 32 bit machine and 16 bytes on a
 * 64 bit machine.  The units of the _SIZE parameters are bytes.
 * Cons pairs and object headers occupy one heap cell.
 *
 * SCM_INIT_HEAP_SIZE is the initial size of heap.  If this much heap is
 * allocated initially the heap will grow by half its current size
 * each subsequent time more heap is needed.
 *
 * If SCM_INIT_HEAP_SIZE heap cannot be allocated initially, SCM_HEAP_SEG_SIZE
 * will be used, and the heap will grow by SCM_HEAP_SEG_SIZE when more
 * heap is needed.  SCM_HEAP_SEG_SIZE must fit into type size_t.  This code
 * is in scm_init_storage() and scm_alloc_some_heap() in sys.c
 * 
 * If SCM_INIT_HEAP_SIZE can be allocated initially, the heap will grow by
 * SCM_EXPHEAP(scm_heap_size) when more heap is needed.
 *
 * SCM_MIN_HEAP_SEG_SIZE is minimum size of heap to accept when more heap
 * is needed.
 *
 * INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
 * trigger a GC. 
 */

#if 0
#  define SCM_INIT_HEAP_SIZE (1024L * 1024L * sizeof(struct scm_cell))
#  define SCM_MIN_HEAP_SEG_SIZE (2048L*sizeof(struct scm_cell))
#  define SCM_HEAP_SEG_SIZE (16384L*sizeof(struct scm_cell))
#  define SCM_EXPHEAP(scm_heap_size) ((524288 <= scm_heap_size) ? 1048576 : scm_heap_size*2)
#  define SCM_INIT_MALLOC_LIMIT SCM_INIT_HEAP_SIZE
#else
#  define SCM_INIT_HEAP_SIZE (1024L * 1024L)
#  define SCM_MIN_HEAP_SEG_SIZE (2048L*sizeof(struct scm_cell))
#  define SCM_HEAP_SEG_SIZE (16384L*sizeof(struct scm_cell))
#  define SCM_EXPHEAP(scm_heap_size) (16384) /* ((scm_n_heap_segs < 2) ? 65536 : ) */
#  define SCM_INIT_MALLOC_LIMIT SCM_INIT_HEAP_SIZE
#endif
/* If fewer than MIN_GC_YIELD cells are recovered during a garbage
 * collection, more space is allocated for the heap.
 */
#define MIN_GC_YIELD (scm_heap_size/4)

/* If fewer than MIN_GC_MALLOC_DELAY ms are spent between garbage
 * collections, more space is allocated for the heap.
 * (NOT CURRENTLY SUPPORTED)
 */
#define MIN_GC_MALLOC_DELAY (scm_gc_rt * 2)





/* CELL_UP and CELL_DN are used by scm_init_heap_seg to find struct scm_cell aligned inner
   bounds for allocated storage */

#ifdef PROT386
/*in 386 protected mode we must only adjust the offset */
# define CELL_UP(p) MK_FP(FP_SEG(p), ~7&(FP_OFF(p)+7))
# define CELL_DN(p) MK_FP(FP_SEG(p), ~7&FP_OFF(p))
#else
# ifdef _UNICOS
#  define CELL_UP(p) (struct scm_cell *)(~1L & ((long)(p)+1L))
#  define CELL_DN(p) (struct scm_cell *)(~1L & (long)(p))
# else
#  define CELL_UP(p) (struct scm_cell *)(~(sizeof(struct scm_cell)-1L) & ((long)(p)+sizeof(struct scm_cell)-1L))
#  define CELL_DN(p) (struct scm_cell *)(~(sizeof(struct scm_cell)-1L) & (long)(p))
# endif				/* UNICOS */
#endif				/* PROT386 */



/* scm_freelist
 * is the head of freelist of cons pairs.
 */
SCM scm_freelist = SCM_EOL;

/* scm_mtrigger
 * is the number of bytes of must_malloc allocation needed to trigger gc.
 */
long scm_mtrigger;

/* scm_gc_heap_lock
 * If set, don't expand the heap.  Set only during gc, during which no allocation
 * is supposed to take place anyway.
 */
int scm_gc_heap_lock = 0;

/* GC Blocking
 * Don't pause for collection if this is set -- just
 * expand the heap.
 */
int scm_block_gc = 1;


/* During collection, this accumulates objects holding
 * weak references.
 */
SCM *scm_weak_vectors;
unsigned int scm_weak_size;
unsigned int scm_n_weak;

/* GC Statistics Keeping
 */
unsigned long scm_cells_allocated = 0;
unsigned long scm_mallocated = 0;
unsigned long scm_gc_cells_collected;
unsigned long scm_gc_malloc_collected;
unsigned long scm_gc_rt;
unsigned long scm_last_gc_time;
unsigned long scm_gc_time_taken = 0;

SCM_SYMBOL (sym_cells_allocated, "cells-allocated");
SCM_SYMBOL (sym_heap_size, "cell-heap-size");
SCM_SYMBOL (sym_mallocated, "bytes-malloced");
SCM_SYMBOL (sym_mtrigger, "gc-malloc-threshold");
SCM_SYMBOL (sym_heap_segments, "cell-heap-segments");
SCM_SYMBOL (sym_gc_time_taken, "gc-time-taken");


struct scm_heap_seg_data
{
  struct scm_cell * bounds[2];	/* lower and upper */
  SCM *freelistp;		/* the value of this may be shared */
  int ncells;			/* per object in this segment */
  int (*valid) ();
};



/* {
  SCM_INTS_UNKNOWN;
Scheme Interface to GC}
 */

/*(c gc-stats)
 * (gc-stats)
 * 
 * Return a list of information from the garbage collector.
 * The format of the list is currently ``unspecified'' and
 * is likely to change in some future release.  Currently,
 * a typical list of gc stats looks like this:
 * 
 *	((gc-time-taken . 4)
 *	 (cells-allocated . 25093)
 *	 (cell-heap-size . 32768)
 *	 (bytes-malloced . 26866)
 *	 (gc-malloc-threshold . 100000)
 *	 (cell-heap-segments (135348224 . 135086080)))
 */
SCM_PROC (s_gc_stats, "gc-stats", 0, 0, 0, scm_gc_stats);
SCM
scm_gc_stats (void)
{
  SCM_INTS_ENABLED;
  int i;
  unsigned int n;
  SCM heap_segs;
  SCM local_scm_mtrigger;
  SCM local_scm_mallocated;
  SCM local_scm_heap_size;
  SCM local_scm_cells_allocated;
  SCM local_scm_gc_time_taken;
  SCM answer;

  SCM_DEFER_INTS;
  scm_block_gc = 1;
 retry:
  heap_segs = SCM_EOL;
  n = scm_n_heap_segs;
  for (i = scm_n_heap_segs; i--; )
    heap_segs = scm_cons (scm_cons (scm_ulong2num ((unsigned long)scm_heap_table[i].bounds[1]),
				    scm_ulong2num ((unsigned long)scm_heap_table[i].bounds[0])),
			  heap_segs);
  if (scm_n_heap_segs != n)
    goto retry;
  scm_block_gc = 0;

  local_scm_mtrigger = scm_mtrigger;
  local_scm_mallocated = scm_mallocated;
  local_scm_heap_size = scm_heap_size;
  local_scm_cells_allocated = scm_cells_allocated;
  local_scm_gc_time_taken = scm_gc_time_taken;

  answer = scm_listify (scm_cons (sym_gc_time_taken, scm_ulong2num (local_scm_gc_time_taken)),
			scm_cons (sym_cells_allocated, scm_ulong2num (local_scm_cells_allocated)),
			scm_cons (sym_heap_size, scm_ulong2num (local_scm_heap_size)),
			scm_cons (sym_mallocated, scm_ulong2num (local_scm_mallocated)),
			scm_cons (sym_mtrigger, scm_ulong2num (local_scm_mtrigger)),
			scm_cons (sym_heap_segments, heap_segs),
			SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return answer;
}


static void 
scm_gc_start ()
{
  SCM_INTS_DISABLED;

  scm_gc_rt = SCM_INUM (scm_get_internal_run_time ());
  scm_last_gc_time = SCM_INUM (scm_get_internal_run_time ());
  scm_gc_cells_collected = 0;
  scm_gc_malloc_collected = 0;
}

static void 
scm_gc_end (void)
{
  SCM_INTS_DISABLED;

  scm_gc_rt = SCM_INUM (scm_get_internal_run_time ()) - scm_gc_rt;
  scm_gc_time_taken = scm_gc_time_taken + scm_gc_rt;
  /* scm_take_signal (scm_gc_signal); */
}


/*(c object-address)
 * (object-address obj)
 * 
 * Return the address in memory or binary representation of `obj'.
 * 
 * This function is mostly useful for debugging.
 */
SCM_PROC(s_object_address, "object-address", 1, 0, 0, scm_object_addr);
SCM
scm_object_addr (SCM obj)
{
  SCM_INTS_UNKNOWN;

  return scm_ulong2num ((unsigned long)obj);
}


/*(c gc)
 * (gc)
 * 
 * Invoke the garbage collector.
 */
SCM_PROC(s_gc, "gc", 0, 0, 0, scm_gc);
SCM 
scm_gc (void)
{
  SCM_DEFER_INTS;
  scm_igc ();
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}



/* {C Interface For When GC is Triggered}
 */

void
scm_gc_for_alloc (int ncells, SCM * freelistp)
{
  SCM_INTS_NESTED;

  SCM_REDEFER_INTS;
  scm_igc ();
  if (   (scm_gc_cells_collected < MIN_GC_YIELD)
      || SCM_IS_IMMEDIATE (*freelistp))
    {
      scm_alloc_some_heap (ncells, freelistp);
    }
  SCM_REALLOW_INTS;
}

void
scm_newcell1 (SCM * into)
{
  if (SCM_IS_IMMEDIATE(scm_freelist))
    *into = scm_gc_for_newcell();
  else
    {
      *into = scm_freelist;
      scm_freelist = SCM_CDR(scm_freelist);
      ++scm_cells_allocated;
    }
}


SCM 
scm_gc_for_newcell (void)
{
  SCM_INTS_NESTED;

  SCM fl;
  scm_gc_for_alloc (1, &scm_freelist);
  fl = scm_freelist;
  scm_freelist = SCM_CDR (fl);
  ++scm_cells_allocated;
  return fl;
}

long 
scm_stack_size (SCM_STACKITEM *start)
{
  SCM_INTS_UNKNOWN;
  SCM_STACKITEM stack;

#ifdef SCM_STACK_GROWS_UP
  return &stack - start;
#else
  return start - &stack;
#endif /* def SCM_STACK_GROWS_UP */
}

void
scm_igc (void)
{
  SCM_INTS_DISABLED;
  jmp_buf save_regs_gc_mark;
  int j;

  scm_gc_start ();
  if (!scm_stack_base || scm_block_gc)
    {
      scm_gc_end ();
      return;
    }

  ++scm_gc_heap_lock;
  scm_n_weak = 0;

  /* Protect from the C stack.
   */
  SCM_FLUSH_REGISTER_WINDOWS;
  /* This assumes that all registers are saved into the jmp_buf */
  setjmp (save_regs_gc_mark);
  scm_mark_locations ((SCM_STACKITEM *) save_regs_gc_mark,
		      (   (size_t) sizeof save_regs_gc_mark
		       / sizeof (SCM_STACKITEM)));

  {
    /* stack_len is long rather than size_t in order to guarantee that
       &stack_len is long aligned */
#ifdef SCM_STACK_GROWS_UP
#ifdef nosve
    long stack_len = (SCM_STACKITEM *) (&stack_len) - scm_stack_base;
#else
    long stack_len = scm_stack_size (scm_stack_base);
#endif
    scm_mark_locations (scm_stack_base, (size_t) stack_len);
#else
#ifdef nosve
    long stack_len = scm_stack_base - (SCM_STACKITEM *) (&stack_len);
#else
    long stack_len = scm_stack_size (scm_stack_base);
#endif
    scm_mark_locations ((scm_stack_base - stack_len), (size_t) stack_len);
#endif
  }

  for (j = 0; j < scm_n_gc_roots; ++j)
    scm_gc_mark (scm_sys_protects[j]);

  scm_gc_mark (scm_rootcont);
  scm_gc_mark (scm_dynwinds);
  scm_gc_mark (scm_progargs);
  scm_gc_mark (scm_exitval);
  scm_gc_mark (scm_cur_inp);
  scm_gc_mark (scm_cur_outp);
  scm_gc_mark (scm_cur_errp);
  scm_gc_mark (scm_cur_loadp);
  scm_gc_mark (scm_def_inp);
  scm_gc_mark (scm_def_outp);
  scm_gc_mark (scm_def_errp);
  scm_gc_mark (scm_top_level_lookup_thunk_var);
  scm_mark_weak_vector_spines ();

  scm_gc_sweep ();

  --scm_gc_heap_lock;
  scm_gc_end ();
}


/* {Mark/Sweep} 
 */

/* Mark an object precisely.
 */
void 
scm_gc_mark (SCM p)
{
  SCM_INTS_DISABLED;
  long i;
  SCM ptr;
  SCM ptr_was;

  ptr = p;
  ptr_was = 0;

gc_mark_loop:
  if (SCM_IS_IMMEDIATE (ptr))
    return;

gc_mark_nimp:
  if (SCM_NCELLP (ptr))
    scm_panic ("rogue pointer during mark");
  ptr_was = ptr;

  switch (SCM_TYP7 (ptr))
    {
    case scm_tcs_cons_nimcar:
      if (SCM_GCMARKP (ptr))
	break;
      SCM_SETGCMARK (ptr);
      if (SCM_IS_IMMEDIATE (SCM_CDR (ptr))) /* SCM_IS_IMMEDIATE works even with a GC mark */
	{
	  ptr = SCM_CAR (ptr);
	  goto gc_mark_nimp;
	}
      scm_gc_mark (SCM_CAR (ptr));
      ptr = SCM_GCCDR (ptr);
      goto gc_mark_nimp;
    case scm_tcs_cons_imcar:
      if (SCM_GCMARKP (ptr))
	break;
      SCM_SETGCMARK (ptr);
      ptr = SCM_GCCDR (ptr);
      goto gc_mark_loop;
    case scm_tcs_cons_gloc:
      if (SCM_GCMARKP (ptr))
	break;
      SCM_SETGCMARK (ptr);
      {
	SCM vcell;
	vcell = SCM_CAR (ptr) - 1L;
	scm_gc_mark (vcell);
	ptr = SCM_GCCDR (ptr);
	goto gc_mark_loop;
      }
      break;
    case scm_tcs_closures:
      if (SCM_GCMARKP (ptr))
	break;
      SCM_SETGCMARK (ptr);
      if (SCM_IS_IMMEDIATE (SCM_CDR (ptr)))
	{
	  ptr = SCM_CLOSCAR (ptr);
	  goto gc_mark_nimp;
	}
      scm_gc_mark (SCM_CLOSCAR (ptr));
      ptr = SCM_GCCDR (ptr);
      goto gc_mark_nimp;
    case scm_tc7_vector:
    case scm_tc7_cclo:
      if (SCM_GC8MARKP (ptr))
	break;
      SCM_SETGC8MARK (ptr);
      i = SCM_LENGTH (ptr);
      if (i == 0)
	break;
      while (--i > 0)
	if (!SCM_IS_IMMEDIATE (SCM_VECTOR_ELTS (ptr)[i]))
	  scm_gc_mark (SCM_VECTOR_ELTS (ptr)[i]);
      ptr = SCM_VECTOR_ELTS (ptr)[0];
      goto gc_mark_loop;
    case scm_tc7_contin:
      if (SCM_GC8MARKP (ptr))
	break;
      SCM_SETGC8MARK (ptr);
      scm_mark_locations (SCM_VECTOR_ELTS (ptr),
			  (size_t) (SCM_LENGTH (ptr) + sizeof (scm_regs) / sizeof (SCM_STACKITEM)));
      break;

    case scm_tc7_string:
    case scm_tc7_static_string:
      SCM_SETGC8MARK (ptr);
      break;

    case scm_tc7_substring:
    case scm_tc7_subsymbol:
    case scm_tc7_static_substring:
    case scm_tc7_static_subsymbol:
      if (SCM_GC8MARKP(ptr))
	break;
      SCM_SETGC8MARK (ptr);
      ptr = SCM_CDR (ptr);
      goto gc_mark_loop;

    case scm_tc7_wvect:
      if (SCM_GC8MARKP(ptr))
	break;
      scm_weak_vectors[scm_n_weak++] = ptr;
      if (scm_n_weak >= scm_weak_size)
	{
	  scm_weak_vectors =
	    (SCM *) realloc ((char *) scm_weak_vectors,
			     sizeof (SCM *) * (scm_weak_size *= 2));
	  if (scm_weak_vectors == NULL)
	    scm_panic ("unable to realloc weak vector table");
	}
      SCM_SETGC8MARK (ptr);
      if (scm_is_weak_hash_table (ptr))
	{
	  int x;
	  int len;
	  int weak_keys;
	  int weak_values;

	  len = SCM_LENGTH (ptr);
	  weak_keys = scm_is_weak_key_hash_table (ptr) || scm_is_doubly_weak_hash_table (ptr);
	  weak_values = scm_is_weak_value_hash_table (ptr) || scm_is_doubly_weak_hash_table (ptr);
	  
	  for (x = 0; x < len; ++x)
	    {
	      SCM alist;
	      alist = SCM_VECTOR_ELTS (ptr)[x];
	      /* mark everything on the alist
	       * except the keys or values, according to weak_values and weak_keys.
	       */
	      while (   !SCM_IS_IMMEDIATE (alist)
		     && SCM_CONSP (alist)
		     && !SCM_GCMARKP (alist)
		     && !SCM_IS_IMMEDIATE (SCM_CAR (alist))
		     && SCM_CONSP (SCM_CAR (alist)))
		{
		  SCM kvpair;
		  SCM next_alist;

		  kvpair = SCM_CAR (alist);
		  next_alist = SCM_CDR (alist);
		  /* 
		   * Do not do this:
		   * 	SCM_SETGCMARK (alist);
		   *	SCM_SETGCMARK (kvpair);
		   *
		   * It may be that either the key or value is protected by
		   * an escaped reference to part of the spine of this alist.
		   * If we mark the spine here, and only mark one or neither of the
		   * key and value, they may never be properly marked.
		   * This leads to a horrible situation in which an alist containing
		   * freelist cells is exported.
		   *
		   * So only mark the spines of these arrays last of all marking.
		   * If somebody confuses us by constructing a weak vector
		   * with a circular alist then we are hosed, but at least we
		   * won't prematurely drop table entries.
		   */
		  if (!weak_keys)
		    scm_gc_mark (SCM_CAR (kvpair));
		  if (!weak_values)
		    scm_gc_mark (SCM_GCCDR (kvpair));
		  alist = next_alist;
		}
	      if (!SCM_IS_IMMEDIATE (alist))
		scm_gc_mark (alist);
	    }
	}
      break;

    case scm_tc7_symbol:
    case scm_tc7_static_symbol:
      if (SCM_GC8MARKP(ptr))
	break;
      SCM_SETGC8MARK (ptr);
      break;
    case scm_tcs_subrs:
      ptr = (SCM)(scm_heap_org + (((unsigned long)SCM_CAR (ptr)) >> 8));
      goto gc_mark_loop;
    case scm_tc7_smob:
      if (SCM_GC8MARKP (ptr))
	break;
      switch SCM_TYP16 (ptr)
	{ /* should be faster than going through scm_smobs */
	case scm_tc_free_cell:
	  /* printf("found free_cell %X ", ptr); fflush(stdout); */
	  SCM_SETGC8MARK (ptr);
	  /* SCM_CDR (ptr) = SCM_EOL; */
	  break;
	case scm_tcs_bignums:
	case scm_tc16_flo:
	  SCM_SETGC8MARK (ptr);
	  break;
	default:
	  i = SCM_SMOBNUM (ptr);
	  if (!(i < scm_numsmob))
	    goto def;
	  ptr = (scm_smobs[i].mark) (ptr);
	  goto gc_mark_loop;
	}
      break;
    default:
    def:scm_panic ("unknown type during mark");
    }
}


/* Mark a Region Conservatively
 */

void 
scm_mark_locations (SCM_STACKITEM x[], size_t n)
{
  SCM_INTS_DISABLED;

  long m = n;
  int i, j;
  struct scm_cell * ptr;

  while (0 <= --m)
    if (SCM_CELLP (*(SCM **) & x[m]))
      {
	ptr = (struct scm_cell *)(*(SCM **) & x[m]);
	i = 0;
	j = scm_n_heap_segs - 1;
	if (   (scm_heap_table[i].bounds[0] <= ptr)
	    && (scm_heap_table[j].bounds[1] > ptr))
	  {
	    while (i <= j)
	      {
		int seg_id;
		seg_id = -1;
		if (   (i == j)
		    || (scm_heap_table[i].bounds[1] > ptr))
		  seg_id = i;
		else if (scm_heap_table[j].bounds[0] <= ptr)
		  seg_id = j;
		else
		  {
		    int k;
		    k = (i + j) / 2;
		    if (k == i)
		      break;
		    if (scm_heap_table[k].bounds[1] > ptr)
		      {
			j = k;
			++i;
			if (scm_heap_table[i].bounds[0] <= ptr)
			  continue;
			else
			  break;
		      }
		    else if (scm_heap_table[k].bounds[0] <= ptr)
		      {
			i = k;
			--j;
			if (scm_heap_table[j].bounds[1] > ptr)
			  continue;
			else
			  break;
		      }
		  }
		if (   !scm_heap_table[seg_id].valid
		    || scm_heap_table[seg_id].valid (ptr,
						     &scm_heap_table[seg_id]))
		  scm_gc_mark (*(SCM *) & x[m]);
		break;
	      }

	  }
      }
}


static void
scm_mark_weak_vector_spines (void)
{
  SCM_INTS_UNKNOWN;
  unsigned int i;

  for (i = 0; i < scm_n_weak; ++i)
    {
      if (scm_is_weak_hash_table (scm_weak_vectors[i]))
	{
	  SCM *ptr;
	  SCM obj;
	  int j;
	  int n;

	  obj = scm_weak_vectors[i];
	  ptr = SCM_VECTOR_ELTS (scm_weak_vectors[i]);
	  n = SCM_LENGTH (scm_weak_vectors[i]);
	  for (j = 0; j < n; ++j)
	    {
	      SCM alist;

	      alist = ptr[j];
	      while (   !SCM_IS_IMMEDIATE (alist)
		     && SCM_CONSP (alist)
		     && !SCM_GCMARKP (alist) 
		     && !SCM_IS_IMMEDIATE (SCM_CAR (alist))
		     && SCM_CONSP  (SCM_CAR (alist)))
		{
		  SCM_SETGCMARK (alist);
		  SCM_SETGCMARK (SCM_CAR (alist));
		  alist = SCM_GCCDR (alist);
		}
	    }
	}
    }
}



static void 
scm_gc_sweep (void)
{
  SCM_INTS_DISABLED;

  struct scm_cell * ptr;
  SCM nfreelist;
  SCM *hp_freelist;
  unsigned long n;
  long m;
  size_t j;
  size_t span;
  unsigned int i;
  size_t seg_size;

  n = 0;
  m = 0;
  i = 0;

  while (i < scm_n_heap_segs)
    {
      hp_freelist = scm_heap_table[i].freelistp;
      nfreelist = SCM_EOL;
      span = scm_heap_table[i].ncells;
      ptr = CELL_UP (scm_heap_table[i].bounds[0]);
      seg_size = CELL_DN (scm_heap_table[i].bounds[1]) - ptr;
      ++i;
      for (j = seg_size + span; j -= span; ptr += span)
	{
	  switch SCM_TYP7 ((SCM)ptr)
	    {
	    case scm_tcs_cons_gloc:
	      if (SCM_GCMARKP ((SCM)ptr))
		{
		  if (SCM_CDR (SCM_CAR ((SCM)ptr) - 1) == (SCM)1)
		    SCM_CDR (SCM_CAR ((SCM)ptr) - 1) = (SCM)0;
		  goto cmrkcontinue;
		}
	      {
		SCM vcell;
		vcell = SCM_CAR ((SCM)ptr) - 1L;

		if ((SCM_CDR (vcell) == 0) || (SCM_CDR (vcell) == 1))
		  {
		    SCM * mem;
		    SCM * block;
		    SCM amt;
		    mem = (SCM *)SCM_CDR ((SCM)ptr);
		    amt = mem[-2];
		    block = (SCM *)mem[-3];
		    free (block);
 		    m += amt * sizeof (SCM);
		  }
	      }
	      break;
	    case scm_tcs_cons_imcar:
	    case scm_tcs_cons_nimcar:
	    case scm_tcs_closures:
	      if (SCM_GCMARKP ((SCM)ptr))
		goto cmrkcontinue;
	      break;
	    case scm_tc7_wvect:
	      if (SCM_GC8MARKP ((SCM)ptr))
		goto c8mrkcontinue;
	      else
		{
		  m += (1 + SCM_LENGTH ((SCM)ptr)) * sizeof (SCM);
		  scm_must_free ((char *)(SCM_VECTOR_ELTS ((SCM)ptr) - 1));
		  break;
		}

	    case scm_tc7_vector:
	    case scm_tc7_cclo:
	      if (SCM_GC8MARKP ((SCM)ptr))
		goto c8mrkcontinue;

	      m += (SCM_LENGTH ((SCM)ptr) * sizeof (SCM));
	    freecdr:
	      scm_must_free ((char *)SCM_CDR ((SCM)ptr));
	      break;
	    case scm_tc7_substring:
	    case scm_tc7_subsymbol:
	    case scm_tc7_static_substring:
	    case scm_tc7_static_subsymbol:
	    case scm_tc7_static_string:
	      if (SCM_GC8MARKP ((SCM)ptr))
		goto c8mrkcontinue;
	      break;
	    case scm_tc7_string:
	      if (SCM_GC8MARKP ((SCM)ptr))
		goto c8mrkcontinue;
	      m += SCM_LENGTH ((SCM)ptr) + 1;
	      goto freecdr;
	    case scm_tc7_symbol:
	      if (SCM_GC8MARKP ((SCM)ptr))
		goto c8mrkcontinue;
	      m += SCM_LENGTH ((SCM)ptr) + 1;
	      goto freecdr;
	    case scm_tc7_static_symbol:
	      if (SCM_GC8MARKP((SCM)ptr))
		goto c8mrkcontinue;
	      break;
	    case scm_tc7_contin:
	      if (SCM_GC8MARKP ((SCM)ptr))
		goto c8mrkcontinue;
	      m += SCM_LENGTH ((SCM)ptr) * sizeof (SCM_STACKITEM) + sizeof (scm_regs);
	      goto freecdr;
	    case scm_tcs_subrs:
	      continue;
	    case scm_tc7_smob:
	      switch (SCM_GCTYP16 ((SCM)ptr))
		{
		case scm_tc_free_cell:
		  if (SCM_GC8MARKP ((SCM)ptr))
		    goto c8mrkcontinue;
		  break;
#ifdef SCM_BIGDIG
		case scm_tcs_bignums:
		  if (SCM_GC8MARKP ((SCM)ptr))
		    goto c8mrkcontinue;
		  m += (SCM_NUMDIGS ((SCM)ptr) * SCM_BITSPERDIG / SCM_CHAR_BIT);
		  goto freecdr;
#endif /* def SCM_BIGDIG */
		case scm_tc16_flo:
		  if (SCM_GC8MARKP ((SCM)ptr))
		    goto c8mrkcontinue;
		  switch ((int) (SCM_CAR ((SCM)ptr) >> 16))
		    {
		    case (SCM_IMAG_PART | SCM_REAL_PART) >> 16:
		      m += sizeof (double);
		    case SCM_REAL_PART >> 16:
		    case SCM_IMAG_PART >> 16:
		      m += sizeof (double);
		      goto freecdr;
		    case 0:
		      break;
		    default:
		      goto sweeperr;
		    }
		  break;
		default:
		  if (SCM_GC8MARKP ((SCM)ptr))
		    goto c8mrkcontinue;

		  {
		    int k;
		    k = SCM_SMOBNUM ((SCM)ptr);
		    if (!(k < scm_numsmob))
		      goto sweeperr;
		    m += (scm_smobs[k].free) ((SCM) (SCM)ptr);
		    break;
		  }
		}
	      break;
	    default:
	    sweeperr:scm_panic ("unknown during sweep");
	    }
	  n += span;
#if 0
	  if (SCM_CAR ((SCM)ptr) == (SCM) scm_tc_free_cell)
	    exit (2);
#endif
	  SCM_CAR ((SCM)ptr) = (SCM) scm_tc_free_cell;
	  SCM_CDR ((SCM)ptr) = nfreelist;
	  nfreelist = (SCM)ptr;
#if 0
	  if ((nfreelist < scm_heap_table[0].bounds[0]) ||
	      (nfreelist >= scm_heap_table[0].bounds[1]))
	    exit (1);
#endif
	  continue;
	c8mrkcontinue:
	  SCM_CLRGC8MARK ((SCM)ptr);
	  continue;
	cmrkcontinue:
	  SCM_CLRGCMARK ((SCM)ptr);
	}
#ifdef SCM_GC_FREE_SEGMENTS
      if (n == seg_size)
	{
	  unsigned int k;
	  scm_heap_size -= seg_size;
	  free ((char *) scm_heap_table[i - 1].bounds[0]);
	  scm_heap_table[i - 1].bounds[0] = 0;
	  for (k = i; k < scm_n_heap_segs; k++)
	    scm_heap_table[k - 1] = scm_heap_table[k];
	  scm_n_heap_segs -= 1;
	  i -= 1;		/* need to scan segment just moved. */
	}
      else
#endif /* ifdef SCM_GC_FREE_SEGMENTS */
	*hp_freelist = nfreelist;

      scm_gc_cells_collected += n;
      n = 0;
    }
  /* Scan weak vectors. */
  {
    SCM * elts;
    for (i = 0; i < scm_n_weak; ++i)
      {
	if (!scm_is_weak_hash_table (scm_weak_vectors[i]))
	  {
	    unsigned int k;
	    elts = SCM_VECTOR_ELTS (scm_weak_vectors[i]);
	    n = SCM_LENGTH (scm_weak_vectors[i]);
	    for (k = 0; k < n; ++k)
	      if (!SCM_IS_IMMEDIATE (elts[k]) && SCM_FREEP (elts[k]))
		elts[k] = SCM_BOOL_F;
	  }
	else /* if (scm_is_weak_hash_table (scm_weak_vectors[i])) */
	  {
	    SCM obj;
	    unsigned int k;
	    obj = scm_weak_vectors[i];
	    elts = SCM_VECTOR_ELTS (scm_weak_vectors[i]);
	    n = SCM_LENGTH (scm_weak_vectors[i]);
	    for (k = 0; k < n; ++k)
	      {
		SCM * fixup;
		SCM alist;
		int weak_keys;
		int weak_values;
		
		weak_keys = scm_is_weak_key_hash_table (obj) || scm_is_doubly_weak_hash_table (obj);
		weak_values = scm_is_weak_value_hash_table (obj) || scm_is_doubly_weak_hash_table (obj);

		fixup = elts + k;
		alist = *fixup;

		while (!SCM_IS_IMMEDIATE (alist)
		       && SCM_CONSP (alist)
		       && !SCM_IS_IMMEDIATE (SCM_CAR (alist))
		       && SCM_CONSP (SCM_CAR (alist)))
		  {
		    SCM key;
		    SCM value;

		    key = SCM_CAAR (alist);
		    value = SCM_CDAR (alist);
		    if (   (weak_keys && !SCM_IS_IMMEDIATE (key) && SCM_FREEP (key))
			|| (weak_values && !SCM_IS_IMMEDIATE (value) && SCM_FREEP (value)))
		      {
			*fixup = SCM_CDR (alist);
		      }
		    else
		      fixup = &SCM_CDR (alist);
		    alist = SCM_CDR (alist);
		  }
	      }
	  }
      }
  }
  scm_cells_allocated = (scm_heap_size - scm_gc_cells_collected);
  scm_mallocated -= m;
  scm_gc_malloc_collected = m;
}




/* {Front end to malloc}
 *
 * scm_must_malloc, scm_must_realloc, scm_must_free
 *
 * These functions provide services comperable to malloc, realloc, and
 * free.  They are for allocating malloced parts of scheme objects.
 * The primary purpose of the front end is to impose calls to gc.
 */

/* scm_must_malloc
 * Return newly malloced storage or throw an error.
 *
 * The parameter WHAT is a string for error reporting.
 * If the threshold scm_mtrigger will be passed by this 
 * allocation, or if the first call to malloc fails,
 * garbage collect -- on the presumption that some objects
 * using malloced storage may be collected.
 *
 * The limit scm_mtrigger may be raised by this allocation.
 */
char *
scm_must_malloc (unsigned long len)
{
  SCM_INTS_DISABLED;
  unsigned long last_time;
  char *ptr;
  size_t size = len;
  long nm = scm_mallocated + size;

  if (len != size)
    {
    malerr:
      scm_panic ("out of memory in scm_must_malloc");
    }
  if ((nm <= scm_mtrigger))
    {
      ptr = (char *) malloc (size);
      if (NULL != ptr)
	{
	  scm_mallocated = nm;
	  return ptr;
	}
    }
  last_time = scm_last_gc_time;
  scm_igc ();
  nm = scm_mallocated + size;
  ptr = (char *) malloc (size);
  if (NULL != ptr)
    {
      scm_mallocated = nm;
      if (nm > scm_mtrigger)
	scm_mtrigger = nm + nm / 2;
#if 0
      else if ((scm_last_gc_time - last_time) < MIN_GC_MALLOC_DELAY)
	scm_mtrigger += scm_mtrigger / 2;
#endif
      return ptr;
    }
  goto malerr;
}


/* scm_must_realloc
 * is similar to scm_must_malloc.
 */
char *
scm_must_realloc (char *where, long olen, unsigned long len)
{
  SCM_INTS_DISABLED;

  char *ptr;
  size_t size;
  long nm;
  size = len;
  nm = scm_mallocated + size - olen;
  if (len != size)
  ralerr:
    scm_panic ("out of memory in scm_must_realloc");
  if ((nm <= scm_mtrigger))
    {
      ptr = (char *) realloc (where, size);
      if (NULL != ptr)
	{
	  scm_mallocated = nm;
	  return ptr;
	}
    }
  scm_igc ();
  nm = scm_mallocated + size - olen;
  ptr = (char *) realloc (where, size);
  if (NULL != ptr)
    {
      scm_mallocated = nm;
      if (nm > scm_mtrigger)
	scm_mtrigger = nm + nm / 2;
      return ptr;
    }
  goto ralerr;
}

/* scm_must_free
 * is for releasing memory from scm_must_realloc and scm_must_malloc.
 */
void 
scm_must_free (char *obj)
{
  SCM_INTS_UNKNOWN;

  if (obj)
    free (obj);
}




/* {Heap Segments}
 *
 * Each heap segment is an array of objects of a particular size.
 * Every segment has an associated (possibly shared) freelist.
 * A table of segment records is kept that records the upper and
 * lower extents of the segment;  this is used during the conservative
 * phase of gc to identify probably gc roots (because they point
 * into valid segments at reasonable offsets).
 */

/* scm_expmem
 * is true if the first segment was smaller than INIT_HEAP_SEG.
 * If scm_expmem is set to one, subsequent segment allocations will
 * allocate segments of size SCM_EXPHEAP(scm_heap_size).
 */
int scm_expmem = 0;

/* scm_heap_org
 * is the lowest base address of any heap segment.
 */
struct scm_cell * scm_heap_org;

struct scm_heap_seg_data * scm_heap_table = 0;
unsigned int scm_n_heap_segs = 0;

/* scm_heap_size
 * is the total number of cells in heap segments.
 */
unsigned long scm_heap_size = 0;

/* init_heap_seg
 * initializes a new heap segment and return the number of objects it contains.
 *
 * The segment origin, segment size in bytes, and the span of objects
 * in cells are input parameters.  The freelist is both input and output.
 *
 * This function presume that the scm_heap_table has already been expanded
 * to accomodate a new segment record.
 */


static size_t 
init_heap_seg (struct scm_cell * seg_org, size_t size, int ncells, SCM *freelistp)
{
  SCM_INTS_UNKNOWN;

  struct scm_cell * ptr;
  struct scm_cell * seg_end;
  size_t new_seg_index;
  size_t n_new_objects;
  
  if (seg_org == NULL)
    return 0;

  ptr = seg_org;

  /* Compute the ceiling on valid object pointers w/in this segment. 
   */
  seg_end = CELL_DN ((char *) ptr + size);

  /* Find the right place and insert the segment record. 
   *
   */
  for (new_seg_index = 0;
       (   (new_seg_index < scm_n_heap_segs)
	&& (scm_heap_table[new_seg_index].bounds[0] <= seg_org));
       new_seg_index++)
    ;

  {
    unsigned int i;
    for (i = scm_n_heap_segs; i > new_seg_index; --i)
      scm_heap_table[i] = scm_heap_table[i - 1];
  }
  
  ++scm_n_heap_segs;

  scm_heap_table[new_seg_index].valid = 0;
  scm_heap_table[new_seg_index].ncells = ncells;
  scm_heap_table[new_seg_index].freelistp = freelistp;
  scm_heap_table[new_seg_index].bounds[0] = (struct scm_cell *)ptr;
  scm_heap_table[new_seg_index].bounds[1] = (struct scm_cell *)seg_end;


  /* Compute the least valid object pointer w/in this segment 
   */
  ptr = CELL_UP (ptr);


  n_new_objects = seg_end - ptr;

  /* Prepend objects in this segment to the freelist. 
   */
  while (ptr < seg_end)
    {
      SCM_CAR ((SCM)ptr) = (SCM) scm_tc_free_cell;
      SCM_CDR ((SCM)ptr) = (SCM) (ptr + ncells);
      ptr += ncells;
    }

  ptr -= ncells;

  /* Patch up the last freelist pointer in the segment
   * to join it to the input freelist.
   */
  SCM_CDR ((SCM)ptr) = *freelistp;
  *freelistp = (SCM)CELL_UP (seg_org);

  scm_heap_size += (ncells * n_new_objects);
  return size;
}


static void 
scm_alloc_some_heap (int ncells, SCM * freelistp)
{
  SCM_INTS_DISABLED;

  struct scm_heap_seg_data * tmptable;
  struct scm_cell * ptr;
  size_t len;

  /* Critical code sections (such as the garbage collector)
   * aren't supposed to add heap segments.
   */
  if (scm_gc_heap_lock)
    scm_panic ("need larger initial heap");

  /* Expand the heap tables to have room for the new segment.
   * Do not yet increment scm_n_heap_segs -- that is done by init_heap_seg
   * only if the allocation of the segment itself succeeds.
   */
  len = (1 + scm_n_heap_segs) * sizeof (struct scm_heap_seg_data);

  tmptable = ((struct scm_heap_seg_data *)
	      realloc ((char *)scm_heap_table, len));
  if (!tmptable)
    scm_panic ("could not grow heap");
  else
    scm_heap_table = tmptable;


  /* Pick a size for the new heap segment.
   * The rule for picking the size of a segment is explained in 
   * gc.h
   */
  if (scm_expmem)
    {
      len = (size_t) (SCM_EXPHEAP (scm_heap_size) * sizeof (struct scm_cell));
      if ((size_t) (SCM_EXPHEAP (scm_heap_size) * sizeof (struct scm_cell)) != len)
	len = 0;
    }
  else
    len = SCM_HEAP_SEG_SIZE;

  {
    size_t smallest;

    smallest = (ncells * sizeof (struct scm_cell));
    if (len < smallest)
      len = (ncells * sizeof (struct scm_cell));

    /* Allocate with decaying ambition. */
    while ((len >= SCM_MIN_HEAP_SEG_SIZE)
	   && (len >= smallest))
      {
	ptr = (struct scm_cell *) malloc (len);
	if (ptr)
	  {
	    init_heap_seg (ptr, len, ncells, freelistp);
	    return;
	  }
	len /= 2;
      }
  }

  scm_panic ("could not grow heap(2)");
}



/*(c unhash-names)
 * (unhash-names names)
 * 
 * `names' is a list of symbols.
 * 
 * This function searches for instances of code compiled by `eval'
 * containing references to top-level variables whose names are
 * included in the `names'.   All such references are ``uncompiled'',
 * meaning that when next evaluated, those variables will be 
 * re-resolved by the module system.
 * 
 * This is mostly useful for robustly re-loading modules.
 * 
 * See xref:"Understanding How Evaluation Works in Systas Scheme".
 * 
 */
SCM_PROC (s_unhash_names, "unhash-names", 1, 0, 0, scm_unhash_names);
SCM
scm_unhash_names (SCM names)
{
  SCM_INTS_ENABLED;
  int x;
  int bound;

  SCM_DEFER_INTS;
  bound = scm_n_heap_segs;
  for (x = 0; x < bound; ++x)
    {
      struct scm_cell * p;
      struct scm_cell * pbound;
      p  = (struct scm_cell *)scm_heap_table[x].bounds[0];
      pbound = (struct scm_cell *)scm_heap_table[x].bounds[1];
      while (p < pbound)
	{
	  SCM incar;
	  incar = p->car;
	  if (1 == (7 & (int)incar))
	    {
	      SCM mq;
	      --incar;
	      mq = scm_memq (SCM_CAR (incar), names);
	      if (   ((names == SCM_BOOL_T) ||
		      (SCM_BOOL_F != mq))
		  && (SCM_CDR (incar) != 0)
		  && (SCM_CDR (incar) != 1))
		{
		  p->car = SCM_CAR (mq);
		}
	    }
	  ++p;
	}
    }
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}




int
scm_init_storage (void)
{
  SCM_INTS_ENABLED;
  unsigned long init_heap_size;
  size_t j;

  init_heap_size = 0;
  for (j = 0; j < scm_n_gc_roots; ++j)
    scm_sys_protects[j] = SCM_BOOL_F;
  scm_block_gc = 1;
  scm_freelist = SCM_EOL;
  scm_expmem = 0;

  j = SCM_HEAP_SEG_SIZE;
  scm_mtrigger = SCM_INIT_MALLOC_LIMIT;
  scm_heap_table = ((struct scm_heap_seg_data *)
		    scm_must_malloc (sizeof (struct scm_heap_seg_data)));
  if (0L == init_heap_size)
    init_heap_size = SCM_INIT_HEAP_SIZE;
  j = init_heap_size;
  if ((init_heap_size != j)
      || !init_heap_seg ((struct scm_cell *) malloc (j), j, 1, &scm_freelist))
    {
      j = SCM_HEAP_SEG_SIZE;
      if (!init_heap_seg ((struct scm_cell *) malloc (j), j, 1, &scm_freelist))
	return 1;
    }
  else
    scm_expmem = 1;
  scm_heap_org = CELL_UP (scm_heap_table[0].bounds[0]);
  /* scm_hplims[0] can change. do not remove scm_heap_org */
  if (!(scm_weak_vectors = (SCM *) malloc ((scm_weak_size = 32) * sizeof(SCM *))))
    return 1;

  scm_undefineds = scm_cons (SCM_UNDEFINED, SCM_EOL);
  SCM_CDR (scm_undefineds) = scm_undefineds;

  scm_listofnull = scm_cons (SCM_EOL, SCM_EOL);
  scm_nullstr = scm_makstr (0L);
  scm_symhash = scm_make_vector (SCM_MAKINUM (scm_symhash_size), SCM_EOL, SCM_UNDEFINED);
  scm_weak_symhash = scm_make_weak_key_hash_table ((SCM) SCM_MAKINUM (scm_symhash_size));
  scm_symhash_vars = scm_make_vector (SCM_MAKINUM (scm_symhash_size), SCM_EOL, SCM_UNDEFINED);
  scm_permobjs = SCM_EOL;
  scm_coextensive_objects = scm_make_weak_value_hash_table (SCM_MAKINUM (61));
  scm_buffer_strings = scm_makvector (61, SCM_BOOL_F, 0, SCM_BOOL_F);
  scm_cpp_constants = scm_make_vector (SCM_MAKINUM (61), SCM_EOL, SCM_BOOL_F);

#ifdef SCM_BIGDIG
  scm_intern_symhash ("bignum-radix", SCM_MAKINUM (SCM_BIGRAD));
#endif
  return 0;
}


/****************************************************************
 * Functions for GC marking
 */


/* scm_mark0
 * 
 * Mark the header of an object with a 7 or 16-bit tag.
 */
SCM 
scm_mark0 (SCM ptr)
{
  SCM_INTS_UNKNOWN;

  SCM_SETGC8MARK (ptr);
  return SCM_BOOL_F;
}


/* scm_mark0
 * 
 * Mark the header of an object with a 7 or 16-bit tag.
 * Return the cdr (for subsequent marking).
 */
SCM 
scm_markcdr (SCM ptr)
{
  SCM_INTS_UNKNOWN;

  if (SCM_GC8MARKP (ptr))
    return SCM_BOOL_F;
  SCM_SETGC8MARK (ptr);
  return SCM_CDR (ptr);
}

size_t 
scm_free0 (SCM ptr __attribute__((unused)))
{
  SCM_INTS_DISABLED;

  return 0;
}



/* {GC Protection Helper Functions}
 */


/* scm_remember
 * 
 * Prevent an otherwise dead variable from being optimized away.
 * This protects the value of the variable from garbage collection.
 * Also see "scm_return_first".
 *
 * In this example, "x" may be garbage collected prematurely because
 * the register containing "x" may be overwritten:
 *
 *		{
 *		   SCM x;
 *		   t_uchar * xc;
 *		   x = scm_makfromstr0 ("hello world");
 *		   xc = SCM_UCHARS (x);
 *
 *		   .... x not used, but xc used ...
 *	           .... x may be overwritten and if GC happens,
 *		        the string may be collected.  In that case,
 *			xc is no longer valid.
 *
 *		   return SCM_BOOL_F;
 *		}
 *
 * In this example, the problem is corrected by fooling the compiler into
 * thinking that x is live throughout the block:
 *
 *		{
 *		   SCM x;
 *		   t_uchar * xc;
 *		   x = scm_makfromstr0 ("hello world");
 *		   scm_remember (&x);
 *		   xc = SCM_UCHARS (x);
 *
 *		   .... x not used, but xc used ...
 *
 *		   return SCM_BOOL_F;
 *		}
 */
SCM scm_junk_ptr;
void
scm_remember (SCM * ptr)
{
  SCM_INTS_UNKNOWN;
  scm_junk_ptr = *ptr;
}

/* scm_return_first
 * 
 * Return the first argument.  Also see "scm_remember".
 *
 * This is used to circumvent certain compiler optimizations.
 * The compiler is free to re-use registers that contain 
 * dead variables.   In this example, "x" may be garbage collected
 * prematurely because the register containing "x" may be overwritten:
 *
 *		{
 *		   SCM x;
 *		   t_uchar * xc;
 *		   x = scm_makfromstr0 ("hello world");
 *		   xc = SCM_UCHARS (x);
 *
 *		   .... x not used, but xc used ...
 *	           .... x may be overwritten and if GC happens,
 *		        the string may be collected.  In that case,
 *			xc is no longer valid.
 *
 *		   return SCM_BOOL_F;
 *		}
 *
 * In this example, the problem is corrected by fooling the compiler into
 * thinking that x is live throughout the block:
 *
 *		{
 *		   SCM x;
 *		   t_uchar * xc;
 *		   x = scm_makfromstr0 ("hello world");
 *		   xc = SCM_UCHARS (x);
 *
 *		   .... x not used, but xc used ...
 *
 *		   return scm_return_first (SCM_BOOL_F, x); // x appears live 
 *		}
 */
SCM
scm_return_first (SCM elt, ...)
{
  SCM_INTS_UNKNOWN;

  return elt;
}


/* scm_permanent_object
 * 
 * Protect an object from garbage collection, forever.
 */
SCM
scm_permanent_object (SCM obj)
{
  SCM_INTS_NESTED;

  SCM_REDEFER_INTS;
  scm_permobjs = scm_cons (obj, scm_permobjs);
  SCM_REALLOW_INTS;
  return obj;
}


/*c
 * SCM scm_coextensive (SCM anchor, SCM obj);
 * 
 * Make `anchor' gc-protect `obj'.
 */
void
scm_coextensive (SCM anchor, SCM obj)
{
  SCM_INTS_ENABLED;

  scm_hashq_set_x (scm_coextensive_objects, anchor, obj);
}




void
scm_init_gc (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/gc.x"
}

