/* malloc.c: 
 *
 ****************************************************************
 * Copyright 1990, 1991, 1992, 1993, 1995, 1996 
 * 	     Free Software Foundation, Inc.
 * 
 * Written May 1989 by Mike Haertel.
 * 
 * Modifications Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/os/errno.h"
#include "hackerlab/os/limits.h"
#include "hackerlab/os/unistd.h"
#include "hackerlab/os/stddef.h"
#include "hackerlab/mem/mem.h"
#include "unexec/libunexec/malloc.h"


/* __STDC__ prototypes for static functions */
static void * align (size_t size);
static void * get_contiguous_space (ptrdiff_t size, void * position);
static void register_heapinfo (void);
static void * morecore (size_t size);
static void free_internal (void * ptr);
static void * realloc_internal (void * ptr, size_t size);
static void * morecore_low (ptrdiff_t increment);



/* This allocator divides the heap into blocks of fixed size; large
 * requests receive one or more whole blocks, and small requests
 * receive a fragment of a block.  Fragment sizes are powers of two,
 * and all fragments of a block are the same size.  When all the
 * fragments in a block have been freed, the block itself is freed.  
 */

#define INT_BIT		(CHAR_BIT * sizeof(int))
#define BLOCKLOG	(INT_BIT > 16 ? 12 : 9)
#define BLOCKSIZE	(1 << BLOCKLOG)
#define BLOCKIFY(SIZE)	(((SIZE) + BLOCKSIZE - 1) / BLOCKSIZE)

/* Determine the amount of memory spanned by the initial heap table
 * (not an absolute limit).  
 */
#define HEAP		(INT_BIT > 16 ? 4194304 : 65536)

/* Number of contiguous free blocks allowed to build up at the end of
 * memory before they will be returned to the system.  
 */
#define FINAL_FREE_BLOCKS	8

/* Data structure giving per-block information.  
 */
union malloc_info
{
  /* Heap information for a busy block.
   */
  struct
  {
    /* Zero for a large (multiblock) object, or positive giving the
     * logarithm to the base two of the fragment size.
     */
    int type;
    union
    {
      struct
      {
	size_t nfree; /* Free frags in a fragmented block.  */
	size_t first; /* First free fragment of the block.  */
      } frag;

      /* For a large object, in its first block, this has the number
       * of blocks in the object.  In the other blocks, this has a
       * negative number which says how far back the first block is.
       */
      ptrdiff_t size;
    } info;
  } busy;

  /* Heap information for a free block
   * (that may be the first of a free cluster).
   */
  struct
  {
    size_t size;	/* Size (in blocks) of a free cluster.  */
    size_t next;	/* Index of next free cluster.  */
    size_t prev;	/* Index of previous free cluster.  */
  } free;
};

/* Address to block number and vice versa.
 */
#define BLOCK(A)	(((char *) (A) - _heapbase) / BLOCKSIZE + 1)
#define ADDRESS(B)	((void *) (((B) - 1) * BLOCKSIZE + _heapbase))

/* Doubly linked lists of free fragments.
 */
struct list
{
  struct list *next;
  struct list *prev;
};

/* List of blocks allocated with `memalign' (or `valloc').
 */
struct alignlist
{
  struct alignlist *next;
  void * aligned;		/* The address that memaligned returned.  */
  void * exact;			/* The address that malloc returned.  */
};

/* Return values for `mprobe': these are the kinds of inconsistencies that
 * `mcheck' enables detection of.
 */
enum mcheck_status
{
  MCHECK_DISABLED = -1,		/* Consistency checking is not turned on.  */
  MCHECK_OK,			/* Block is fine.  */
  MCHECK_FREE,			/* Block freed twice.  */
  MCHECK_HEAD,			/* Memory before the block was clobbered.  */
  MCHECK_TAIL			/* Memory after the block was clobbered.  */
};

/* Statistics available to the user.
 */
struct mstats
{
  size_t bytes_total;		/* Total size of the heap. */
  size_t chunks_used;		/* Chunks allocated by the user. */
  size_t bytes_used;		/* Byte total of user-allocated chunks. */
  size_t chunks_free;		/* Chunks in the free list. */
  size_t bytes_free;		/* Byte total of chunks in the free list. */
};

/* Pointer to the base of the first block.
 */
static char *_heapbase;

/* Block information table.  Allocated with align/__free (not malloc/free).
 */
static union malloc_info *_heapinfo;

/* Number of info entries.
 */
static size_t heapsize;

/* Search index in the info table.
 */
static size_t _heapindex;

/* Limit of valid info table indices.
 */
static size_t _heaplimit;

/* Free lists for each fragment size.
 */
static struct list _fraghead[BLOCKLOG];

/* Instrumentation.
 */
static size_t _chunks_used;
static size_t _bytes_used;
static size_t _chunks_free;
static size_t _bytes_free;

/* Are you experienced?
 */
static int __malloc_initialized;
static size_t __malloc_extra_blocks;


/* Aligned allocation.
 */

static void *
align (size_t size)
{
  void * result;
  unsigned long int adj;

  result = morecore_low (size);
  adj = (unsigned long int) ((unsigned long int) ((char *) result - (char *) 0)) % BLOCKSIZE;

  if (adj != 0)
    {
      void * new;

      adj = BLOCKSIZE - adj;
      new = morecore_low (adj);
      result = (char *) result + adj;
    }

  return result;
}

/* Get SIZE bytes, if we can get them starting at END.
 * Return the address of the space we got.
 * If we cannot get space at END, fail and return 0.
 */
static void *
get_contiguous_space (ptrdiff_t size, void * position)
{
  void * before;
  void * after;

  before = morecore_low (0);

  /* If we can tell in advance that the break is at the wrong place,
   * fail now.
   */

  if (before != position)
    return 0;

  /* Allocate SIZE bytes and get the address of them.
   */
  after = morecore_low (size);
  if (!after)
    return 0;

  /* It was not contiguous -- reject it.
   */
  if (after != position)
    {
      morecore_low (- size);
      return 0;
    }

  return after;
}


/* This is called when `_heapinfo' and `heapsize' have just
 * been set to describe a new info table.  Set up the table
 * to describe itself and account for it in the statistics.
 */
static void
register_heapinfo (void)
{
  size_t block, blocks;

  block = BLOCK (_heapinfo);
  blocks = BLOCKIFY (heapsize * sizeof (union malloc_info));

  /* Account for the _heapinfo block itself in the statistics.
   */
  _bytes_used += blocks * BLOCKSIZE;
  ++_chunks_used;

  /* Describe the heapinfo block itself in the heapinfo.
   */
  _heapinfo[block].busy.type = 0;
  _heapinfo[block].busy.info.size = blocks;

  /* Leave back-pointers for malloc_find_address.
   */
  while (--blocks > 0)
    _heapinfo[block + blocks].busy.info.size = -blocks;
}

/* Set everything up and remember that we have.
 */
static int
__malloc_initialize ()
{
  if (__malloc_initialized)
    return 0;

  heapsize = HEAP / BLOCKSIZE;

  _heapinfo = (union malloc_info *) align (heapsize * sizeof (union malloc_info));
  if (_heapinfo == 0)
    return 0;

  mem_set ((t_uchar *)_heapinfo, 0, heapsize * sizeof (union malloc_info));

  _heapinfo[0].free.size = 0;
  _heapinfo[0].free.next = _heapinfo[0].free.prev = 0;
  _heapindex = 0;
  _heapbase = (char *) _heapinfo;
  _heaplimit = BLOCK (_heapbase + heapsize * sizeof (union malloc_info));

  register_heapinfo ();

  __malloc_initialized = 1;
  return 1;
}

static int morecore_recursing;

/* Get neatly aligned memory, initializing or
 * growing the heap info table as necessary.
 */
static void *
morecore (size_t size)
{
  void * result;
  union malloc_info * newinfo;
  union malloc_info * oldinfo;
  size_t newsize;

  /* Avoid recursion.  The caller will know how to handle a 0 return.
   */
  if (morecore_recursing)
    return 0;

  result = align (size);
  if (result == 0)
    return 0;

  /* Check if we need to grow the info table.
   */
  if ((size_t) BLOCK ((char *) result + size) > heapsize)
    {
      /* Calculate the new _heapinfo table size.  We do not account for the
       * added blocks in the table itself, as we hope to place them in
       * existing free space, which is already covered by part of the
       * existing table.
       */
      newsize = heapsize;

      do
	{
	  newsize *= 2;
	}
      while ((size_t) BLOCK ((char *) result + size) > newsize);

      /* We must not reuse existing core for the new info table when called
       * from realloc in the case of growing a large block, because the
       * block being grown is momentarily marked as free.  In this case
       * _heaplimit is zero so we know not to reuse space for internal
       * allocation.
       */
      if (_heaplimit != 0)
	{
	  int save;

	  /* First try to allocate the new info table in core we already
	   * have, in the usual way using realloc.  If realloc cannot
	   * extend it in place or relocate it to existing sufficient core,
	   * we will get called again, and the code above will notice the
	   * `morecore_recursing' flag and return 0.
	   */

	  save = errno;		/* Don't want to clobber errno with ENOMEM.  */
	  morecore_recursing = 1;
	  newinfo = (union malloc_info *) realloc_internal
	    (_heapinfo, newsize * sizeof (union malloc_info));
	  morecore_recursing = 0;

	  if (newinfo == 0)
	    errno = save;
	  else
	    {
	      /* We found some space in core, and realloc has put the old
	       * table's blocks on the free list.  Now zero the new part
	       * of the table and install the new table location.
	       */
	      mem_set ((t_uchar *)&newinfo[heapsize], 0, (newsize - heapsize) * sizeof (union malloc_info));
	      _heapinfo = newinfo;
	      heapsize = newsize;
	      goto got_heap;
	    }
	}

      /* Allocate new space for the malloc info table.
       */
      while (1)
  	{
 	  newinfo = (union malloc_info *) align (newsize * sizeof (union malloc_info));

 	  /* Did it fail?  */
 	  if (newinfo == 0)
 	    {
 	      morecore_low (-size);
 	      return 0;
 	    }

 	  /* Is it big enough to record status for its own space?
 	   *  If so, we win.
	   */
 	  if ((size_t) BLOCK ((char *) newinfo + newsize * sizeof (union malloc_info)) < newsize)
 	    break;

 	  /* Must try again.  First give back most of what we just got.
	   */
 	  morecore_low (- newsize * sizeof (union malloc_info));
 	  newsize *= 2;
  	}

      /* Copy the old table to the beginning of the new,
       * and zero the rest of the new table.
       */
      mem_move ((t_uchar *)newinfo, (t_uchar *) _heapinfo, heapsize * sizeof (union malloc_info));
      mem_set ((t_uchar *)&newinfo[heapsize], 0, (newsize - heapsize) * sizeof (union malloc_info));
      oldinfo = _heapinfo;
      _heapinfo = newinfo;
      heapsize = newsize;

      register_heapinfo ();

      /* Reset _heaplimit so free_internal never decides
       * it can relocate or resize the info table.
       */
      _heaplimit = 0;
      free_internal (oldinfo);

      /* The new heap limit includes the new table just allocated.
       */
      _heaplimit = BLOCK ((char *) newinfo + heapsize * sizeof (union malloc_info));
      return result;
    }

 got_heap:
  _heaplimit = BLOCK ((char *) result + size);
  return result;
}

/* Allocate memory from the heap.
 */
static void *
_malloc_internal (size_t size)
{
  void * result;
  size_t block;
  size_t blocks;
  size_t lastblocks;
  size_t start;
  register size_t i;
  struct list *next;

  /* ANSI C allows `malloc (0)' to either return 0, or to return a
   * valid address you can realloc and free (though not dereference).
   * 
   * It turns out that some extant code (sunrpc, at least Ultrix's version)
   * expects `malloc (0)' to return non-0 and breaks otherwise.
   * Be compatible.
   */

  if (size < sizeof (struct list))
    size = sizeof (struct list);

  /* Determine the allocation policy based on the request size.
   */
  if (size <= BLOCKSIZE / 2)
    {
      /* Small allocation to receive a fragment of a block.
       * Determine the logarithm to base two of the fragment size.
       */
      size_t log;

      log = 1;
      --size;

      while ((size /= 2) != 0)
	++log;

      /* Look in the fragment lists for a
       * free fragment of the desired size.
       */
      next = _fraghead[log].next;

      if (next != 0)
	{
	  /* There are free fragments of this size.
	   * Pop a fragment out of the fragment list and return it.
	   * Update the block's nfree and first counters.
	   */

	  result = (void *) next;

	  next->prev->next = next->next;
	  if (next->next != 0)
	    next->next->prev = next->prev;

	  block = BLOCK (result);

	  if (--_heapinfo[block].busy.info.frag.nfree != 0)
	    _heapinfo[block].busy.info.frag.first = (unsigned long int)((unsigned long int) ((char *) next->next - (char *) 0) % BLOCKSIZE) >> log;

	  /* Update the statistics.
	   */
	  ++_chunks_used;
	  _bytes_used += 1 << log;
	  --_chunks_free;
	  _bytes_free -= 1 << log;
	}
      else
	{
	  /* No free fragments of the desired size, so get a new block
	   * and break it into fragments, returning the first.
	   */
	  result = malloc (BLOCKSIZE);
	  if (result == 0)
	    return 0;

	  /* Link all fragments but the first into the free list.
	   */
	  next = (struct list *) ((char *) result + (1 << log));
	  next->next = 0;
	  next->prev = &_fraghead[log];
	  _fraghead[log].next = next;

	  for (i = 2; i < (size_t) (BLOCKSIZE >> log); ++i)
	    {
	      next = (struct list *) ((char *) result + (i << log));
	      next->next = _fraghead[log].next;
	      next->prev = &_fraghead[log];
	      next->prev->next = next;
	      next->next->prev = next;
	    }

	  /* Initialize the nfree and first counters for this block.
	   */
	  block = BLOCK (result);
	  _heapinfo[block].busy.type = log;
	  _heapinfo[block].busy.info.frag.nfree = i - 1;
	  _heapinfo[block].busy.info.frag.first = i - 1;

	  _chunks_free += (BLOCKSIZE >> log) - 1;
	  _bytes_free += BLOCKSIZE - (1 << log);
	  _bytes_used -= BLOCKSIZE - (1 << log);
	}
    }
  else
    {
      /* Large allocation to receive one or more blocks.
       * Search the free list in a circle starting at the last place visited.
       * If we loop completely around without finding a large enough
       * space we will have to get more memory from the system.
       */
      blocks = BLOCKIFY (size);
      start = block = _heapindex;

      while (_heapinfo[block].free.size < blocks)
	{
	  block = _heapinfo[block].free.next;

	  if (block == start)
	    {
	      /* Need to get more from the system.  Get a little extra.
	       */
	      size_t wantblocks = blocks + __malloc_extra_blocks;
	      block = _heapinfo[0].free.prev;
	      lastblocks = _heapinfo[block].free.size;
	      /* Check to see if the new core will be contiguous with the
	       * final free block; if so we don't need to get as much.
	       * We can't do this if we will have to make the heap info
	       * table bigger to accomodate the new space.
	       */
	      if (_heaplimit != 0
		  && block + lastblocks == _heaplimit
		  && block + wantblocks <= heapsize
		  && get_contiguous_space ((wantblocks - lastblocks) * BLOCKSIZE, ADDRESS (block + lastblocks)))
		{
 		  /* We got it contiguously.  Which block we are extending
		   * (the `final free block' referred to above) might have
		   * changed, if it got combined with a freed info table.
		   */
 		  block = _heapinfo[0].free.prev;
  		  _heapinfo[block].free.size += (wantblocks - lastblocks);
		  _bytes_free += (wantblocks - lastblocks) * BLOCKSIZE;
 		  _heaplimit += wantblocks - lastblocks;
		  continue;
		}
	      result = morecore (wantblocks * BLOCKSIZE);
	      if (result == 0)
		return 0;
	      block = BLOCK (result);
	      /* Put the new block at the end of the free list.
	       */
	      _heapinfo[block].free.size = wantblocks;
	      _heapinfo[block].free.prev = _heapinfo[0].free.prev;
	      _heapinfo[block].free.next = 0;
	      _heapinfo[0].free.prev = block;
	      _heapinfo[_heapinfo[block].free.prev].free.next = block;
	      ++_chunks_free;
	      /* Now loop to use some of that block for this allocation.
	       */
	    }
	}

      /* At this point we have found a suitable free list entry.
       * Figure out how to remove what we need from the list.
       */
      result = ADDRESS (block);
      if (_heapinfo[block].free.size > blocks)
	{
	  /* The block we found has a bit left over,
	   * so relink the tail end back into the free list.
	   */
	  _heapinfo[block + blocks].free.size = _heapinfo[block].free.size - blocks;
	  _heapinfo[block + blocks].free.next = _heapinfo[block].free.next;
	  _heapinfo[block + blocks].free.prev = _heapinfo[block].free.prev;
	  _heapinfo[_heapinfo[block].free.prev].free.next
	    = _heapinfo[_heapinfo[block].free.next].free.prev
	    = _heapindex
	    = block + blocks;
	}
      else
	{
	  /* The block exactly matches our requirements,
	   * so just remove it from the list.
	   */
	  _heapinfo[_heapinfo[block].free.next].free.prev = _heapinfo[block].free.prev;
	  _heapinfo[_heapinfo[block].free.prev].free.next = _heapindex = _heapinfo[block].free.next;
	  --_chunks_free;
	}

      _heapinfo[block].busy.type = 0;
      _heapinfo[block].busy.info.size = blocks;
      ++_chunks_used;
      _bytes_used += blocks * BLOCKSIZE;
      _bytes_free -= blocks * BLOCKSIZE;

      /* Mark all the blocks of the object just allocated except for the
       * first with a negative number so you can find the first block by
       * adding that adjustment.
       */
      while (--blocks > 0)
	_heapinfo[block + blocks].busy.info.size = -blocks;
    }

  return result;
}

void *
malloc (size_t size)
{
  if (!__malloc_initialized && !__malloc_initialize ())
    return 0;

  return  _malloc_internal (size);
}

/* On some ANSI C systems, some libc functions call _malloc, _free
 * and _realloc.  Make them use the GNU functions.
 */

void *
_malloc (size_t size)
{
  return malloc (size);
}

void
_free (void * ptr)
{
  free (ptr);
}

void *
_realloc (void * ptr, size_t size)
{
  return realloc (ptr, size);
}


/* List of blocks allocated by memalign.
 */
static struct alignlist *_aligned_blocks = 0;

/* Return memory to the heap.
 */
static void
free_internal (void * ptr)
{
  int type;
  size_t block, blocks;
  register size_t i;
  struct list * prev;
  struct list * next;
  void * curbrk;
  /* Threshold of free space at which we will return some to the system.
   */
  const size_t lesscore_threshold = FINAL_FREE_BLOCKS + 2 * __malloc_extra_blocks;
  struct alignlist *l;

  if (ptr == 0)
    return;

  for (l = _aligned_blocks; l != 0; l = l->next)
    {
      if (l->aligned == ptr)
	{
	  l->aligned = 0;	/* Mark the slot in the list as free.  */
	  ptr = l->exact;
	  break;
	}
    }

  block = BLOCK (ptr);

  type = _heapinfo[block].busy.type;

  switch (type)
    {
    case 0:
      /* Get as many statistics as early as we can.
       */
      --_chunks_used;
      _bytes_used -= _heapinfo[block].busy.info.size * BLOCKSIZE;
      _bytes_free += _heapinfo[block].busy.info.size * BLOCKSIZE;

      /* Find the free cluster previous to this one in the free list.
       * Start searching at the last block referenced; this may benefit
       * programs with locality of allocation.
       */
      i = _heapindex;
      if (i > block)
	{
	  while (i > block)
	    i = _heapinfo[i].free.prev;
	}
      else
	{
	  do
	    {
	      i = _heapinfo[i].free.next;
	    }
	  while (i > 0 && i < block);
	  i = _heapinfo[i].free.prev;
	}

      /* Determine how to link this block into the free list.
       */
      if (block == i + _heapinfo[i].free.size)
	{
	  /* Coalesce this block with its predecessor.
	   */
	  _heapinfo[i].free.size += _heapinfo[block].busy.info.size;
	  block = i;
	}
      else
	{
	  /* Really link this block back into the free list.
	   */
	  _heapinfo[block].free.size = _heapinfo[block].busy.info.size;
	  _heapinfo[block].free.next = _heapinfo[i].free.next;
	  _heapinfo[block].free.prev = i;
	  _heapinfo[i].free.next = block;
	  _heapinfo[_heapinfo[block].free.next].free.prev = block;
	  ++_chunks_free;
	}

      /* Now that the block is linked in, see if we can coalesce it
       * with its successor (by deleting its successor from the list
       * and adding in its size).
       */
      if (block + _heapinfo[block].free.size == _heapinfo[block].free.next)
	{
	  _heapinfo[block].free.size += _heapinfo[_heapinfo[block].free.next].free.size;
	  _heapinfo[block].free.next = _heapinfo[_heapinfo[block].free.next].free.next;
	  _heapinfo[_heapinfo[block].free.next].free.prev = block;
	  --_chunks_free;
	}

      /* How many trailing free blocks are there now?
       */
      blocks = _heapinfo[block].free.size;

      /* Where is the current end of accessible core?
       */
      curbrk = morecore_low (0);

      if (_heaplimit != 0 && curbrk == ADDRESS (_heaplimit))
	{
 	  size_t info_block;
 	  size_t info_blocks;
 	  size_t prev_block;
 	  size_t prev_blocks;
 	  size_t next_block;
 	  size_t next_blocks;

	  /* The end of the malloc heap is at the end of accessible core.
	   * It's possible that moving _heapinfo will allow us to
	   * return some space to the system.
	   */

 	  info_block = BLOCK (_heapinfo);
 	  info_blocks = _heapinfo[info_block].busy.info.size;
 	  prev_block = _heapinfo[block].free.prev;
 	  prev_blocks = _heapinfo[prev_block].free.size;
 	  next_block = _heapinfo[block].free.next;
 	  next_blocks = _heapinfo[next_block].free.size;


	  if (/* Win if this block being freed is last in core, the info table
	       * is just before it, the previous free block is just before the
	       * info table, and the two free blocks together form a useful
	       * amount to return to the system.
	       */
	      (block + blocks == _heaplimit &&
	       info_block + info_blocks == block &&
	       prev_block != 0 && prev_block + prev_blocks == info_block &&
	       blocks + prev_blocks >= lesscore_threshold)

	      ||
	      /* Nope, not the case.  We can also win if this block being
	       * freed is just before the info table, and the table extends
	       * to the end of core or is followed only by a free block,
	       * and the total free space is worth returning to the system.
	       */
	      (block + blocks == info_block &&
	       ((info_block + info_blocks == _heaplimit &&
		 blocks >= lesscore_threshold) ||
		(info_block + info_blocks == next_block &&
		 next_block + next_blocks == _heaplimit &&
		 blocks + next_blocks >= lesscore_threshold))))
	    {
	      union malloc_info *newinfo;
	      size_t oldlimit = _heaplimit;

	      /* Free the old info table, clearing _heaplimit to avoid
	       * recursion into this code.  We don't want to return the
	       * table's blocks to the system before we have copied them to
	       * the new location.
	       */
	      _heaplimit = 0;
	      free_internal (_heapinfo);
	      _heaplimit = oldlimit;

	      /* Tell malloc to search from the beginning of the heap for
	       * free blocks, so it doesn't reuse the ones just freed.
	       */
	      _heapindex = 0;

	      /* Allocate new space for the info table and move its data.
	       */
	      newinfo = (union malloc_info *) _malloc_internal (info_blocks * BLOCKSIZE);
	      mem_move ((t_uchar *)newinfo, (t_uchar *)_heapinfo, info_blocks * BLOCKSIZE);
	      _heapinfo = newinfo;

	      /* We should now have coalesced the free block with the
	       * blocks freed from the old info table.  Examine the entire
	       * trailing free block to decide below whether to return some
	       * to the system.
	       */
	      block = _heapinfo[0].free.prev;
	      blocks = _heapinfo[block].free.size;
 	    }

	  /* Now see if we can return stuff to the system.
	   */
	  if (block + blocks == _heaplimit && blocks >= lesscore_threshold)
	    {
	      register size_t bytes = blocks * BLOCKSIZE;
	      _heaplimit -= blocks;
	      morecore_low (-bytes);
	      _heapinfo[_heapinfo[block].free.prev].free.next = _heapinfo[block].free.next;
	      _heapinfo[_heapinfo[block].free.next].free.prev = _heapinfo[block].free.prev;
	      block = _heapinfo[block].free.prev;
	      --_chunks_free;
	      _bytes_free -= bytes;
	    }
	}

      /* Set the next search to begin at this block.
       */
      _heapindex = block;
      break;

    default:
      /* Do some of the statistics.
       */
      --_chunks_used;
      _bytes_used -= 1 << type;
      ++_chunks_free;
      _bytes_free += 1 << type;

      /* Get the address of the first free fragment in this block.
       */
      prev = (struct list *) ((char *) ADDRESS (block) + (_heapinfo[block].busy.info.frag.first << type));

      if (_heapinfo[block].busy.info.frag.nfree == (BLOCKSIZE >> type) - 1)
	{
	  /* If all fragments of this block are free, remove them
	   * from the fragment list and free the whole block.
	   */
	  next = prev;
	  for (i = 1; i < (size_t) (BLOCKSIZE >> type); ++i)
	    next = next->next;
	  prev->prev->next = next;
	  if (next != 0)
	    next->prev = prev->prev;
	  _heapinfo[block].busy.type = 0;
	  _heapinfo[block].busy.info.size = 1;

	  /* Keep the statistics accurate.
	   */
	  ++_chunks_used;
	  _bytes_used += BLOCKSIZE;
	  _chunks_free -= BLOCKSIZE >> type;
	  _bytes_free -= BLOCKSIZE;

	  free (ADDRESS (block));
	}
      else if (_heapinfo[block].busy.info.frag.nfree != 0)
	{
	  /* If some fragments of this block are free, link this
	   * fragment into the fragment list after the first free
	   * fragment of this block.
	   */
	  next = (struct list *) ptr;
	  next->next = prev->next;
	  next->prev = prev;
	  prev->next = next;
	  if (next->next != 0)
	    next->next->prev = next;
	  ++_heapinfo[block].busy.info.frag.nfree;
	}
      else
	{
	  /* No fragments of this block are free, so link this
	   * fragment into the fragment list and announce that
	   * it is the first free fragment of this block.
	   */
	  prev = (struct list *) ptr;
	  _heapinfo[block].busy.info.frag.nfree = 1;
	  _heapinfo[block].busy.info.frag.first = (unsigned long int)((unsigned long int) ((char *) ptr - (char *) 0) % BLOCKSIZE >> type);
	  prev->next = _fraghead[type].next;
	  prev->prev = &_fraghead[type];
	  prev->prev->next = prev;
	  if (prev->next != 0)
	    prev->next->prev = prev;
	}
      break;
    }
}

/* Return memory to the heap.
 */
void
free (void * ptr)
{
  free_internal (ptr);
}

void
cfree (void * ptr)
{
  free (ptr);
}

#define min(A, B) ((A) < (B) ? (A) : (B))

/* Resize the given region to the new size, returning a pointer
 * to the (possibly moved) region.  This is optimized for speed;
 * some benchmarks seem to indicate that greater compactness is
 * achieved by unconditionally allocating and copying to a
 * new region.  This module has incestuous knowledge of the
 * internals of both free and malloc.
 */
static void *
realloc_internal (void * ptr, size_t size)
{
  void * result;
  int type;
  size_t block;
  size_t blocks;
  size_t oldlimit;

  if (size == 0)
    {
      free_internal (ptr);
      return _malloc_internal (0);
    }
  else if (ptr == 0)
    return _malloc_internal (size);

  block = BLOCK (ptr);

  type = _heapinfo[block].busy.type;
  switch (type)
    {
    case 0:
      /* Maybe reallocate a large block to a small fragment.
       */
      if (size <= BLOCKSIZE / 2)
	{
	  result = _malloc_internal (size);
	  if (result != 0)
	    {
	      mem_move (result, ptr, size);
	      free_internal (ptr);
	      return result;
	    }
	}

      /* The new size is a large allocation as well;
       * see if we can hold it in place.
       */
      blocks = BLOCKIFY (size);
      if (blocks < _heapinfo[block].busy.info.size)
	{
	  /* The new size is smaller; return
	   * excess memory to the free list.
	   */
	  _heapinfo[block + blocks].busy.type = 0;
	  _heapinfo[block + blocks].busy.info.size = _heapinfo[block].busy.info.size - blocks;
	  _heapinfo[block].busy.info.size = blocks;
	  /* We have just created a new chunk by splitting a chunk in two.
	   * Now we will free this chunk; increment the statistics counter
	   * so it doesn't become wrong when free_internal decrements it.
	   */
	  ++_chunks_used;
	  free_internal (ADDRESS (block + blocks));
	  result = ptr;
	}
      else if (blocks == _heapinfo[block].busy.info.size)
	{
	  /* No size change necessary.
	   */
	  result = ptr;
	}
      else
	{
	  /* Won't fit, so allocate a new region that will.
	   * Free the old region first in case there is sufficient
	   * adjacent free space to grow without moving.
	   */
	  blocks = _heapinfo[block].busy.info.size;
	  /* Prevent free from actually returning memory to the system.
	   */
	  oldlimit = _heaplimit;
	  _heaplimit = 0;
	  free_internal (ptr);
	  result = _malloc_internal (size);
	  if (_heaplimit == 0)
	    _heaplimit = oldlimit;
	  if (result == 0)
	    {
	      /* Now we're really in trouble.  We have to unfree
	       * the thing we just freed.  Unfortunately it might
	       * have been coalesced with its neighbors.
	       */
	      if (_heapindex == block)
		{
		  (void) _malloc_internal (blocks * BLOCKSIZE);
		}
	      else
		{
		  void * previous;

		  previous = _malloc_internal ((block - _heapindex) * BLOCKSIZE);
		  (void) _malloc_internal (blocks * BLOCKSIZE);
		  free_internal (previous);
		}
	      return 0;
	    }
	  if (ptr != result)
	    {
	      mem_move (result, ptr, blocks * BLOCKSIZE);
	    }
	}
      break;

    default:
      /* Old size is a fragment; type is logarithm
       * to base two of the fragment size.
       */
      if (size > (size_t) (1 << (type - 1)) && size <= (size_t) (1 << type))
	{
	  /* The new size is the same kind of fragment.
	   */
	  result = ptr;
	}
      else
	{
	  /* The new size is different; allocate a new space,
	   * and copy the lesser of the new size and the old.
	   */
	  result = _malloc_internal (size);
	  if (result == 0)
	    return 0;
	  mem_move (result, ptr, min (size, (size_t) 1 << type));
	  free_internal (ptr);
	}
      break;
    }

  return result;
}

void *
realloc (void * ptr, size_t size)
{
  if (!__malloc_initialized && !__malloc_initialize ())
    return 0;

  return realloc_internal (ptr, size);
}

/* Allocate an array of NMEMB elements each SIZE bytes long.
 * The entire array is initialized to zeros.
 */
void *
calloc (size_t nmemb, size_t size)
{
  void * result;

  result = malloc (nmemb * size);

  if (result != 0)
    (void) mem_set (result, 0, nmemb * size);

  return result;
}


void *
memalign (size_t alignment, size_t size)
{
  void * result;
  unsigned long int adj;
  unsigned long int lastadj;

  /* Allocate a block with enough extra space to pad the block with up to
   * (ALIGNMENT - 1) bytes if necessary.
   */
  result = malloc (size + alignment - 1);
  if (result == 0)
    return 0;

  /* Figure out how much we will need to pad this particular block
   * to achieve the required alignment.
   */
  adj = (unsigned long int) ((char *) result - (char *) 0) % alignment;

  do
    {
      /* Reallocate the block with only as much excess as it needs.
       */
      free (result);
      result = malloc (adj + size);
      if (result == 0)	/* Impossible unless interrupted.  */
	return 0;

      lastadj = adj;
      adj = (unsigned long int) ((char *) result - (char *) 0) % alignment;
      /* It's conceivable we might have been so unlucky as to get a
       * different block with weaker alignment.  If so, this block is too
       * short to contain SIZE after alignment correction.  So we must
       * try again and get another block, slightly larger.
       */
    } while (adj > lastadj);

  if (adj != 0)
    {
      /* Record this block in the list of aligned blocks, so that `free'
       * can identify the pointer it is passed, which will be in the middle
       * of an allocated block.
       */

      struct alignlist *l;
      for (l = _aligned_blocks; l != 0; l = l->next)
	{
	  if (l->aligned == 0)
	    {
	      /* This slot is free.  Use it.
	       */
	      break;
	    }
	}
      if (l == 0)
	{
	  l = (struct alignlist *) malloc (sizeof (struct alignlist));
	  if (l == 0)
	    {
	      free (result);
	      return 0;
	    }
	  l->next = _aligned_blocks;
	  _aligned_blocks = l;
	}
      l->exact = result;
      result = l->aligned = (char *) result + alignment - adj;
    }

  return result;
}

static size_t pagesize;

void *
valloc (size_t size)
{
  if (pagesize == 0)
    pagesize = getpagesize ();

  return memalign (pagesize, size);
}




/* Allocate INCREMENT more bytes of data space,
   and return the start of data space, or 0 on errors.
   If INCREMENT is negative, shrink data space.  */
static void *
morecore_low (ptrdiff_t increment)
{
  void * result;

  result = (void *) sbrk (increment);
  if (result == (void *) -1)
    return 0;
  return result;
}

/* tag: Tom Lord Mon May 13 02:04:18 2002 (malloc.c)
 */
