/* unit-mem.c - test mem.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/bugs/test-coverage.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-mem";
static t_uchar * usage = "[options]";
static t_uchar * version_string = "1.0";

#define OPTS(OP, OP2) \
  OP (opt_help_msg, "h", "help", 0, \
      "Display a help message and exit.") \
  OP (opt_version, "V", "version", 0, \
      "Display a release identifier string") \
  OP2 (opt_version, 0, 0, 0, "and exit.")

enum options
{
  OPTS (OPT_ENUM, OPT_IGN)  
};

struct opt_desc opts[] = 
{
  OPTS (OPT_DESC, OPT_DESC)
    {-1, 0, 0, 0, 0}
};



#define HASH8(X)	((((X) >> 8) & 0xff) ^ ((X) & 0xff))

int
main (int argc, char * argv[])
{
  int o;
  struct opt_parsed * option;

  option = 0;
  while (1)
    {
      o = opt_standard (lim_use_must_malloc, &option, opts, &argc, argv, program_name, usage, version_string, 0, opt_help_msg, opt_none, opt_version);
      if (o == opt_none)
	break;
      switch (o)
	{
	default:
	  safe_printfmt (2, "unhandled option `%s'\n", option->opt_string);
	  panic ("internal error parsing arguments");
	}
    }

  safe_printfmt (1, "  mem_set tests...\n");
  {
    t_uchar buffer[1024];
    int from;
    int to;
    
    for (from = 0; from < 13; ++from)
      for (to = from; to < from + 64; ++to)
	{
	  int x;
	  mem_set (buffer, '1', sizeof (buffer));
	  mem_set (buffer + from, '2', to - from);

	  for (x = 0; x < sizeof (buffer); ++x)
	    invariant (((x >= from) && (x < to))
		       ? (buffer[x] == '2')
		       : (buffer[x] == '1'));
	}
  }
  safe_printfmt (1, "  mem_set0 tests...\n");
  {
    t_uchar buffer[1024];
    int from;
    int to;
    
    for (from = 0; from < 13; ++from)
      for (to = from; to < from + 64; ++to)
	{
	  int x;
	  mem_set (buffer, '1', sizeof (buffer));
	  mem_set0 (buffer + from, to - from);
	  
	  for (x = 0; x < sizeof (buffer); ++x)
	    invariant (((x >= from) && (x < to))
		       ? (buffer[x] == 0)
		       : (buffer[x] == '1'));
	}
  }
  safe_printfmt (1, "  mem_move tests...\n");
  {
    t_uchar buffer[1024];
    int left;
    int right;
    int size;
    
    for (left = 0; left < 13; ++left)
      for (right = left; right < left + 32; ++right)
	for (size = 0; size < 64; ++size)
	  {
	    int x;
	    
	    for (x = 0; x < sizeof (buffer); ++x)
	      buffer[x] = HASH8 (x);
	    
	    mem_move (buffer + left, buffer + right, size);
	    
	    for (x = 0; x < sizeof (buffer); ++x)
	      {
		invariant (((x >= left) && (x < (left + size)))
			   ? (buffer[x] == HASH8 (right + (x - left)))
			   : (buffer[x] == HASH8 (x)));
	      }


	    for (x = 0; x < sizeof (buffer); ++x)
	      buffer[x] = HASH8 (x);
	    
	    mem_move (buffer + right, buffer + left, size);
	    
	    for (x = 0; x < sizeof (buffer); ++x)
	      {
		invariant (((x >= right) && (x < (right + size)))
			   ? (buffer[x] == HASH8 (left + (x - right)))
			   : (buffer[x] == HASH8 (x)));
	      }
	  }
  }

  safe_printfmt (1, "  mem_cmp tests...\n");
  {
    t_uchar buffer[1024];
    int left;
    int right;
    int size;
    
    for (left = 0; left < 13; ++left)
      for (right = left; right < left + 32; ++right)
	for (size = 0; size < 64; ++size)
	  {
	    int x;
	    int cmp;
	    
	    for (x = 0; x < sizeof (buffer); ++x)
	      buffer[x] = HASH8 (x);
	    
	    cmp = 0;
	    for (x = 0; x < size; ++x)
	      if (buffer[left + x] < buffer[right + x])
		{
		  cmp = -1;
		  break;
		}
	      else if (buffer[left + x] > buffer[right + x])
		{
		  cmp = 1;
		  break;
		}
	    
	    invariant (cmp == mem_cmp (buffer + left, buffer + right, size));
	    invariant ((-cmp) == mem_cmp (buffer + right, buffer + left, size));
	  }
  }

  safe_printfmt (1, "  mem_occurrences tests...\n");
  {
    t_uchar buffer[1024];
    int x;
    
    for (x = 0; x < sizeof (buffer); ++x)
      buffer[x] = HASH8 (x);
	    
    for (x = 0; x < 256; ++x)
      {
	int occur;
	int y;

	occur = 0;
	for (y = 0; y < sizeof (buffer); ++y)
	  {
	    if (buffer[y] == x)
	      ++occur;
	  }

	invariant (occur == mem_occurrences (buffer, x, sizeof (buffer)));
      }

    {
      t_uchar buffer[1024];
      int from;
      int to;
    
      for (from = 0; from < 13; ++from)
	for (to = from; to < from + 64; ++to)
	  {
	    int y;
	    int occur;

	    x = random () % 256;

	    occur = 0;
	    for (y = from; y < to; ++y)
	      {
		if (buffer[y] == x)
		  ++occur;
	      }

	    invariant (occur == mem_occurrences (buffer + from, x, to - from));
	  }
    }

  }
  
  return 0;
}



