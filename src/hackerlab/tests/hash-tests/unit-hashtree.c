/* unit-hashtree.c - test hashtree.c
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/math.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/hash/hashtree.h"
#include "hackerlab/cmd/main.h"



static t_uchar * program_name = "unit-hashtree";
static t_uchar * usage = "[options] < tests > results";
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




static void
parse_uint (int * answer, t_uchar ** line, long * len)
{
  int errn;
  t_uchar * start;
  
  while (*len && char_is_space (**line))
    {
      ++*line;
      --*len;
    }

  start = *line;

  while (*len && char_is_digit (**line))
    {
      ++*line;
      --*len;
    }

  if (start == *line)
    panic ("syntax error in input");

  if (cvt_decimal_to_uint (&errn, answer, start, (*line - start)))
    panic ("overflow error in number");

  while (*len && char_is_space (**line))
    {
      ++*line;
      --*len;
    }
}

static void
count_depths (int * n_items, float * total_depth, struct hashtree * ht, int depth)
{
  int x;

  if (!ht)
    return;

  for (x = 0; x < 16; ++x)
    if (bitset_is_member (&ht->nested_p, x))
      count_depths (n_items, total_depth, (struct hashtree *)ht->children[x], 1 + depth);
    else if (ht->children[x])
      {
	struct hashtree_item * item;

	item = (struct hashtree_item *)ht->children[x];
	while (item)
	  {
	    *n_items += 1;
	    *total_depth += (float)depth;
	    item = item->next_same_hash;
	  }
      }
}


#define ABS(X) (((X) < 0) ? -(X) : (X))

static void
count_deviation (float * answer, struct hashtree * ht, float average_depth, int depth)
{
  int x;

  if (!ht)
    return;

  for (x = 0; x < 16; ++x)
    if (bitset_is_member (&ht->nested_p, x))
      count_deviation (answer, (struct hashtree *)ht->children[x], average_depth, 1 + depth);
    else if (ht->children[x])
      {
	struct hashtree_item * item;

	item = (struct hashtree_item *)ht->children[x];
	while (item)
	  {
	    *answer += ABS((float)depth - average_depth);
	    item = item->next_same_hash;
	  }
      }
}


static int
round_fi (double f)
{
  double n;

  /* f >= 0 */

  if (modf (f, &n) < 0.5)
    return (int)f + 1;
  else
    return (int)f;
}


int
main (int argc, char * argv[])
{
  int errn;
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


  {
    struct hashtree * ht;

    ht = hashtree_alloc (0);
    if (!ht)
      panic ("unable to allocate hash tree");
    
    while (1)
      {
	long len;
	t_uchar * line;

	if (0 > vfdbuf_next_line (&errn, &line, &len, 0))
	  panic ("vfdbuf_next_line error");
	if (len == 0)
	  exit (0);
	else if (len < 0)
	  panic ("input error");
	else
	  {
	    int c;
	    int key;
	    int val;
	    struct hashtree_item * item;


	    c = *line;
	    ++line;
	    --len;
	    switch (c)
	      {
	      default:
		panic ("command syntax error");
	      case 's':
		parse_uint (&key, &line, &len);
		parse_uint (&val, &line, &len);
		item = hashtree_store (ht, (unsigned long)key, (void *)key, 0);
		item->binding = (void *)val;
		break;
	      case 'f':
		parse_uint (&key, &line, &len);
		item = hashtree_find (ht, (unsigned long)key, (void *)key, 0);
		if (!item)
		  safe_printfmt (1, "%d unbound\n", key);
		else
		  safe_printfmt (1, "%d => %d\n", key, (int)item->binding);
		break;
	      case 'd':
		parse_uint (&key, &line, &len);
		item = hashtree_find (ht, (unsigned long)key, (void *)key, 0);
		if (item)
		  hashtree_delete (item, 0);
		break;
	      case 'q':
		{
		  int n_items;
		  float total_depth;
		  float average_depth;
		  float total_deviation;
		  float average_deviation;

		  n_items = 0;
		  total_depth = 0.0;
		  count_depths (&n_items, &total_depth, ht, 0);
		  average_depth = total_depth / (float)n_items;
		  total_deviation = 0.0;
		  count_deviation (&total_deviation, ht, average_depth, 0);
		  average_deviation = total_deviation / (float)n_items;
		  /* safe_printfmt (1, "%d items, average depth %f, average depth deviation %f\n", n_items, average_depth, average_deviation); */
		  safe_printfmt (2, "%d items, average depth %d, average depth deviation %d\n", n_items, (int)round_fi (average_depth), (int)round_fi (average_deviation));
		  exit (0);
		}
	      }
	  }
      }
  }
  return 0;
}

