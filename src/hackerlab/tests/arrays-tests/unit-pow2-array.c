/* tag: Tom Lord Tue Dec  4 14:40:35 2001 (unit-pow2-array.c)
 */
/* unit-pow2-array.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/cmd/main.h"
#include "hackerlab/arrays/pow2-array-compact.h"



#define array_size	0x10000

static int normal_array[array_size];

struct test_array
{
  pow2_array_rules rules;
  pow2_array array;
};

static struct test_array test_arrays[4];

static int default_page[array_size];


int
main (int argc, char * argv[])
{
  int default_value;
  int x;
  int y;
  int z;

  default_value = (random () % INT_MAX);
  for (x = 0; x < array_size; ++x)
    {
      normal_array[x] = default_value;
      default_page[x] = default_value;
    }

  test_arrays[0].rules = make_pow2_array_rules (lim_use_must_malloc, sizeof (int), (void *)default_page, 15, 1, 0, 0x7fff);
  test_arrays[1].rules = make_pow2_array_rules (lim_use_must_malloc, sizeof (int), (void *)default_page, 13, 7, 9, 0xf, 0, 0x1ff);
  test_arrays[2].rules = make_pow2_array_rules (lim_use_must_malloc, sizeof (int), (void *)default_page, 12, 0xf, 8, 0xf, 0, 0xff);
  test_arrays[3].rules = make_pow2_array_rules (lim_use_must_malloc, sizeof (int), (void *)default_page, 12, 0xf, 8, 0xf, 6, 3, 0, 0x3f);

  for (y = 0; y < 4; ++y)
    test_arrays[y].array = pow2_array_alloc (lim_use_must_malloc, test_arrays[y].rules);

  for (x = 0; x <= (2 * array_size); ++x)
    {
      if ((x % 0x100) == 0)
	{
	  if ((x % 0x1000) == 0)
	    safe_printfmt (1, "...%d\n", x);
	  for (y = 0; y < array_size; ++y)
	    {
	      int reference;

	      reference = normal_array[y];
	      for (z = 0; z < 4; ++z)
		{
		  if (reference != (*(int *)pow2_array_rref (test_arrays[z].array, y)))
		    {
		      safe_printfmt (2, "Array %d differs at position %d at iteration %d\n", z, y, x);
		      exit (1);
		    }
		}
	    }
	}

      {
	int pos;
	int new_value;

	pos = (random () % array_size);
	new_value = (random () % INT_MAX);
	normal_array[pos] = new_value;
	for (y = 0; y < 4; ++y)
	  {
	    *(int *)pow2_array_ref (test_arrays[y].array, pos) = new_value;
	  }
      }
    }

  safe_printfmt (1, "testing pow2_array_compact...\n");
  for (z = 0; z < 4; ++z)
    pow2_array_compact (test_arrays[z].array, 0, 0, 0);
  for (y = 0; y < array_size; ++y)
    {
      int reference;
      
      reference = normal_array[y];
      for (z = 0; z < 4; ++z)
	{
	  if (reference != (*(int *)pow2_array_rref (test_arrays[z].array, y)))
	    {
	      safe_printfmt (2, "Array %d differs at position %d at iteration %d\n", z, y, x);
	      exit (1);
	    }
	}
    }
  safe_printfmt (1, "completed pow2-array tests successfully");
  exit (0);
}

