/* dbug.c - debugging routines for rx
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/char/char-name.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/uni/coding.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/rx/dbug.h"


/************************************************************************
 *(h0 "Rx Debugging Functions" 
 *    :include ("rx/dbug.h"))
 * 
 * 
 * The functions in this section are for printing debugging information
 * from applications that use Rx.
 */



#ifdef HAVE_POSITIONAL_ARRAY_INITS
#define AT(X) [X] =
#else
#define AT(X)
#endif

#define RX_EXP_NODE_TYPE_NAME(X)	#X,
static char *node_type_names[] =
{
  RX_EXP_NODE_TYPES(NAME)
};


static void
print_cset_char (int fd, int x)
{
  int errn;

  if ((x == ']') || (x == '[') || (x == '-') || (x == '^'))
    printfmt (&errn, fd, "\\%c", x);
  else if (x < 256)
    printfmt (&errn, fd, "%s", char_name[x]);
  else
    printfmt (&errn, fd, "\\u%04x", x);
}

static void
print_cset (int fd, int cset_size, bits cs)
{
  int x;
  int errn;

  if (!cs)
    printfmt (&errn, fd, "nil");
  else
    {
      printfmt (&errn, fd, "[");


      x = 0;
      while (x < cset_size)
	{
	  int y;

	  x = bits_ffs_range (cs, x, cset_size);
	  if (x < 0)
	    break;
	  y = bits_ffc_range (cs, x, cset_size);
	  if (y < 0)
	    y = cset_size + 1;

	  print_cset_char (fd, x);
	  if ((y - 1) != x)
	    {
	      printfmt (&errn, fd, "-");
	      print_cset_char (fd, (y - 1));
	    }
	  x = y;
	}
      printfmt (&errn, fd, "]");
    }
}


static void
print_string (int fd, enum uni_encoding_scheme encoding, t_uchar * str, size_t str_len, t_uchar bracket)
{
  int errn;

  if (!str && bracket)
    printfmt (&errn, fd, "nil");
  else
    {
      size_t pos;

      if (bracket)
	printfmt (&errn, fd, "\"");

      pos = 0;
      while (pos < str_len)
	{
	  t_unicode c;

	  c = (uni_encoding_iscan_fn (encoding)) (str, &pos, str_len);
	  if (c < 256)
	    printfmt (&errn, fd, "%s", char_name[c]);
	  else
	    print_cset_char (fd, c);
	}

      if (bracket)
	printfmt (&errn, fd, "\"");
    }
}


static void
spaces (int fd, int n)
{
  int errn;
  while (n--)
    printfmt (&errn, fd, " ");
}


/*(c rx_print_rexp)
 * void rx_print_rexp (int fd,
 *		       int cset_size,
 *		       int indent,
 *		       struct rx_exp_node * rexp);
 * 
 * Print the expression `rexp' as a list of nodes, using indenting to
 * indicate subexpression nesting.  See xref:"Regexp Expression
 * Trees".
 * 
 * `fd' is the descriptor on which to print the expression.
 * 
 * `cset_size' is the character set size of the expression (usually
 * 256).
 * 
 * `indent' is the initial level of indenting for the printed
 * expression (usually 0).
 */
void
rx_print_rexp (int fd,
	       int cset_size,
	       int indent,
	       struct rx_exp_node * rexp)
{
  int errn;

  spaces (fd, indent);
  if (!rexp)
    printfmt (&errn, fd, "nil\n");
  else
    {
      printfmt (&errn, fd,
		"Node %lx type %d (%s), iv=%ld(%c), iv2=%ld, len=%ld obs=%d max_paren=%d, min_paren=%d cs=",
		(unsigned long) rexp,
		rexp->type,
		node_type_names[rexp->type],
		rexp->intval,
		((   (0 <= rexp->intval)
		  && (256 > rexp->intval)
		  && char_is_printable (rexp->intval))
		 ? (char)rexp->intval
		 : ' '),
		rexp->intval2,
		rexp->len,
		rexp->observed,
		rexp->max_enclosed_paren,
		rexp->min_enclosed_paren);
      print_cset (fd, cset_size, rexp->cset);
      printfmt (&errn, fd, " s=");
      print_string (fd, rexp->encoding, rexp->str, rexp->str_len, 1);
      printfmt (&errn, fd, "\n");
      if (rexp->left || rexp->right)
	{
	  rx_print_rexp (fd, cset_size, indent + 2, rexp->left);
	  rx_print_rexp (fd, cset_size, indent + 2, rexp->right);
	}
    }
}


/*(c rx_unparse_print_rexp)
 * void rx_unparse_print_rexp (int fd,
 *			       int cset_size,
 *			       struct rx_exp_node * rexp);
 * 
 * Print the expression `rexp' using regexp syntax.
 * See xref:"Regexp Expression Trees".
 */
void
rx_unparse_print_rexp (int fd,
		       int cset_size,
		       struct rx_exp_node * rexp)
{
  int errn;

  if (!rexp)
    return;
  else
    switch (rexp->type)
      {
      case r_cset:
	if (1 != bits_population (rexp->cset))
	  print_cset (fd, cset_size, rexp->cset);
	else
	  {
	    int x;

	    x = bits_ffs (rexp->cset);

	    print_cset_char (fd, x);
	  }
	break;

      case r_string:
	print_string (fd, rexp->encoding, rexp->str, rexp->str_len, 0);
	break;

      case r_parens:
	printfmt (&errn, fd, "(");
	rx_unparse_print_rexp (fd, cset_size, rexp->left);
	printfmt (&errn, fd, ")");
	break;

      case r_context:
	printfmt (&errn, fd, "\\%c", (char)rexp->intval);
	break;

      case r_cut:
	printfmt (&errn, fd, "[[:cut %ld:]]", rexp->intval);
	break;

      case r_concat:
	rx_unparse_print_rexp (fd, cset_size, rexp->left);
	rx_unparse_print_rexp (fd, cset_size, rexp->right);
	break;

      case r_right_concat:
	rx_unparse_print_rexp (fd, cset_size, rexp->left);
	rx_unparse_print_rexp (fd, cset_size, rexp->right);
	break;

      case r_alternate:
	rx_unparse_print_rexp (fd, cset_size, rexp->left);
	printfmt (&errn, fd, "|");
	rx_unparse_print_rexp (fd, cset_size, rexp->right);
	break;

      case r_star:
	rx_unparse_print_rexp (fd, cset_size, rexp->left);
	printfmt (&errn, fd, "*");
	break;

      case r_interval:
	rx_unparse_print_rexp (fd, cset_size, rexp->left);
	printfmt (&errn, fd, "{%ld,%ld}", rexp->intval, rexp->intval2);
	break;
      }
}


static void
rx_print_nfa_state (int fd, struct rx_nfa * rx, struct rx_nfa_state * state)
{
  int errn;
  struct rx_nfa_edge * e;

  printfmt (&errn, fd,
	    "state %d, state_label %lu, is_start %d\n",
	    state->id, state->state_label, state->is_start);

  for (e = state->edges; e; e = e->next)
    {
      printfmt (&errn, fd,
		"\tEdge %s to %d ",
		(e->type == ne_cset
		 ? "cset"
		 : (e->type == ne_epsilon
		    ? "epsilon"
		    : "side effect")),
		e->dest->id);

      if (e->type == ne_cset)
	print_cset (fd, rx->local_cset_size, e->cset);
      else
	printfmt (&errn, fd, "epsilon");
      printfmt (&errn, fd, "\n");
    }
}


/*(c rx_print_nfa)
 * void rx_print_nfa (int fd, struct rx_nfa * rx);
 * 
 * Print the NFA `rx' as a list of nodes and edges.  See
 * xref:"Non-deterministic Finite-state Automata".
 * 
 * `fd' is the descriptor on which to print the NFA.
 */
void
rx_print_nfa (int fd, struct rx_nfa * rx)
{
  struct rx_nfa_state * state;

  for (state = rx->nfa_states; state; state = state->next)
    rx_print_nfa_state (fd, rx, state);
}


/*(c rx_print_superstate)
 * void rx_print_superstate (int fd, struct rx_superstate * state);
 * 
 * Print the DFA state `state' as a list of the NFA states that it
 * contains.  See xref:"The Superstate DFA".
 * 
 * `fd' is the descriptor on which to print the NFA.
 */
void
rx_print_superstate (int fd, struct rx_superstate * state)
{
  struct rx_superset * set;
  int errn;

  printfmt (&errn, fd, "superstate: ");
  set = state->members;
  while (set->car)
    {
      printfmt (&errn, fd, "%d ", set->car->id);
      set = set->cdr;
    }
  printfmt (&errn, fd, "\n");
}
