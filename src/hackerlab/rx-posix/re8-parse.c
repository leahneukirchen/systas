/* re8-parse.c - compiling regexp syntax to rx_exp_node trees
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/setjmp.h"
#include "hackerlab/os/char-class-locale.h"
#include "hackerlab/os/char-cmp-locale.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/rx/bits-tree-rules.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx-posix/posix.h"
#include "hackerlab/rx-posix/re8-parse.h"


/****************************************************************
 *(h0 "Regexp Parsing"
 *    :includes ("rx-posix/re8-parse.h"))
 * 
 * The functions in this section compile Posix BRE ("basic regular
 * expression") and ERE ("extended regular expression") syntax to
 * expression trees.
 * 
 * (The standard names of these syntaxes are a mistake: the pattern
 * languages they describe are not regular expressions but regexps.
 * See *xref*.)
 */

/* Functions Used Internally by the Parser 
 */

struct rx_parse_state
{
  struct rx_exp_node * root;

  t_uchar const * pos;

  t_uchar const * pattern;
  int size;
  int extended_p;
  int newline_separates_lines;
  int cset_size;

  const t_uchar * translate;
  bits inv_tr[256];
  int n_members [256];

  int at_beg;
  char backrefs_valid[10];
  int exp_number;

  int dfa_only;
  int cut_count;

  int err;
  jmp_buf err_escape;
};


static void rx_parse_alt (struct rx_exp_node ** where,
			  struct rx_parse_state * state,
			  int paren_matched);
static void rx_parse_concat (struct rx_exp_node ** where,
			     struct rx_parse_state * state,
			     int paren_matched);
static void rx_parse_repeated (struct rx_exp_node ** where,
			       struct rx_parse_state * state,
			       int paren_matched);
static void rx_parse_item (struct rx_exp_node ** where,
			   struct rx_parse_state * state,
			   int paren_matched);

static void
cleanup_parse_state (struct rx_parse_state * state)
{
  int x;
  for (x = 0; x < 256; ++x)
    {
      if (state->inv_tr[x])
	bits_free (state->inv_tr[x]);
    }
}


static int
rx_eop (struct rx_parse_state * state)
{
  return (state->pos == (state->pattern + state->size));
}


static int
rx_scan_ahead (struct rx_parse_state * state, t_uchar * token, int len)
{
  if ((state->pos + len) > (state->pattern + state->size))
    return 0;

  {
    int x;
    for (x = 0; x < len; ++x)
      if (state->translate[*(state->pos + x)] != token[x])
	return 0;
    return 1;
  }
}


static int
rx_expand_op (t_uchar * buf, struct rx_parse_state * state, int op)
{
  if (!state->extended_p && (op != '*'))
    {
      buf[0] = '\\';
      buf[1] = op;
      return 2;
    }
  else
    {
      buf[0] = op;
      return 1;
    }
}


static int
rx_scan_op_ahead (struct rx_parse_state * state, int op)
{
  int x;
  t_uchar buf[2];
  x = rx_expand_op (buf, state, op);
  return rx_scan_ahead (state, buf, x);
}


static int
rx_scan (struct rx_parse_state * state, t_uchar * token, int len)
{
  if (rx_scan_ahead (state, token, len))
    {
      state->pos += len;
      return 1;
    }
  else
    return 0;
}


static int
rx_scan_op (struct rx_parse_state * state, int op)
{
  int x;
  t_uchar buf[2];
  x = rx_expand_op (buf, state, op);
  return rx_scan (state, buf, x);
}


static int
rx_factor_string (struct rx_exp_node *** lastp, int cset_size)
{
  struct rx_exp_node ** expp;
  struct rx_exp_node * exp;
  bits cs;
  struct rx_exp_node * cset_node;

  expp = *lastp;
  exp = *expp;

  cs = bits_alloc (rx_nfa_cache_limits (), rx_8bit_bits_tree_rule);
  if (!cs)
    {
      return REG_ESPACE;
    }
  if (bits_adjoin (cs, exp->str[exp->str_len - 1]))
    {
      bits_free (cs);
      return REG_ESPACE;
    }
  cset_node = rx_mk_r_cset_take (r_cset, cset_size, cs);
  if (!cset_node)
    {
      bits_free (cs);
      return REG_ESPACE;
    }
  cset_node->observed = 0;
  if (exp->str_len == 1)
    {
      rx_free_exp (exp);
      *expp = cset_node;
      return 0;
    }
  else
    {
      struct rx_exp_node * concat_node;
      concat_node = rx_mk_r_binop (r_concat, exp, cset_node);
      if (!concat_node)
	{
	  rx_free_exp (cset_node);
	  return REG_ESPACE;
	}
      exp->str_len--;
      concat_node->observed = 0;
      *expp = concat_node;
      *lastp = &concat_node->right;
      return 0;
    }
}


/* The compiler keeps an inverted translation table.
 * This looks up/inititalize elements.
 * VALID is an array of booleans that validate CACHE.
 */
static bits
rx_inverse_translation (int * n_members,
			int cset_size,
			bits * inv_tr,
			const t_uchar * translate,
			int c)
{
  if (!inv_tr[c])
    {
      bits cs;
      int x;
      int c_tr;
      int membs;

      inv_tr[c] = bits_alloc (rx_nfa_cache_limits(), rx_8bit_bits_tree_rule);
      if (!inv_tr[c])
	return 0;
      cs = inv_tr[c];

      c_tr = translate[(t_uchar)c];
      if (bits_clear (cs))
	return 0;
      membs = 0;
      for (x = 0; x < 256; ++x)
	if (translate[x] == c_tr)
	  {
	    if (bits_adjoin (cs, x))
	      return 0;
	    membs++;
	  }
      n_members[c] = membs;
    }
  return inv_tr[c];
}



/* A Recursive Descent Regexp Parser 
 */

static void
rx_parse_alt (struct rx_exp_node ** where,
	      struct rx_parse_state * state,
	      int paren_matched)
{
  rx_parse_concat (where, state, paren_matched);

  if (rx_scan_op (state, '|'))
    {
      struct rx_exp_node * alt;
      
      alt = rx_mk_r_binop (r_alternate, *where, 0);
      if (!alt)
	{
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      state->at_beg = 1;
      *where = alt;
      rx_parse_alt (&alt->right, state, paren_matched);
      alt->observed = ((   alt->left
			&& alt->left->observed)
		       || (   alt->right
			   && alt->right->observed));
    }
}


static void
rx_parse_concat (struct rx_exp_node ** where,
		 struct rx_parse_state * state,
		 int paren_matched)
{
  rx_parse_repeated (where, state, paren_matched);

  while (   *where
	    && !rx_eop (state)
	    && !rx_scan_op_ahead (state, '|')
	    && !((paren_matched == 1) && rx_scan_op_ahead (state, ')'))
	    && !((paren_matched == 2) && rx_scan_ahead (state, "):]]", 4)))
    {
      struct rx_exp_node * concat;

      concat = rx_mk_r_binop (r_concat, *where, 0);
      if (!concat)
	{
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      *where = concat;
      rx_parse_repeated (&concat->right, state, paren_matched);
      concat->observed = ((   concat->left
			   && concat->left->observed)
			  || (   concat->right
			      && concat->right->observed));
    }
}


static void
rx_parse_repeated (struct rx_exp_node ** where,
		   struct rx_parse_state * state,
		   int paren_matched)
{
  char const * saved_pos;
  enum { plus, opt, star, interval } op_type;
  int iv;
  int iv2;

  rx_parse_item (where, state, paren_matched);

  if (!state->extended_p && state->at_beg)
    return;

  saved_pos = state->pos;

  if (rx_scan_op (state, '+'))
    op_type = plus;
  else if (rx_scan_op (state, '?'))
    op_type = opt;
  else if (rx_scan_op (state, '*'))
    op_type = star;
  else if (!state->dfa_only && rx_scan_op (state, '{'))
    {
      int lo;
      int hi;
      int errn;
      t_uchar const * start;
      t_uchar const * bound;

      bound = state->pattern + state->size;
      if (state->pos == bound)
	{
	not_an_interval:
	  state->err = REG_BADBR;
	  longjmp (state->err_escape, 1);
	}

      start = state->pos;
      while (   (state->pos < bound)
	     && (char_is_digit (state->translate[*state->pos])))
	++state->pos;

      if (cvt_decimal_to_int (&errn, &lo, (t_uchar *)start, state->pos - start))
	{
	  state->err = REG_BADBR;
	  longjmp (state->err_escape, 1);
	}

      if (state->pos == bound)
	goto not_an_interval;

      if (rx_scan (state, ",", 1))
	{
	  start = state->pos;
 	  while (   (state->pos < bound)
		 && (char_is_digit (state->translate[*state->pos])))
	    ++state->pos;

	  if (state->pos == start)
	    hi = -1;
	  else if (cvt_decimal_to_int (&errn, &hi, (t_uchar *)start, state->pos - start))
	    {
	      state->err = REG_BADBR;
	      longjmp (state->err_escape, 1);
	    }
	  if (!(rx_scan_op (state, '}')))
	    goto not_an_interval;
	  goto know_range;
	}
      else if (rx_scan_op (state, '}'))
	{
	  hi = lo;
	  goto know_range;
	}
      else
	goto not_an_interval;
      
    know_range:
      if (((hi < lo) && (hi >= 0)) || (hi > RX_DUP_MAX))
	{
	  state->err = REG_BADBR;
	  longjmp (state->err_escape, 1);
	}
      op_type = interval;
      iv = lo;
      iv2 = hi;
    }
  else
    return;

  while (*where)
    {
      /* repeat operators apply to an entire r_right_concat,
       * but that could change in the future if r_right_concat
       * can be generated by something other than rx_parse_repeated.
       */
      if (((*where)->type == r_concat))
	where = &(*where)->right;
      else if ((*where)->type == r_string)
	{
	  int err;
	  err = rx_factor_string (&where, state->cset_size);
	  if (err)
	    {
	      state->err = REG_ESPACE;
	      longjmp (state->err_escape, 1);
	    }
	}
      else
	break;
    }

  {
    struct rx_exp_node * iter;

    switch (op_type)
      {
      case opt:
	{
	  struct rx_exp_node * alt;
	  alt = rx_mk_r_binop (r_alternate, *where, 0);
	  if (!alt)
	    {
	      state->err = REG_ESPACE;
	      longjmp (state->err_escape, 1);
	    }
	  alt->observed = (*where) && ((*where)->observed);
	  *where = alt;
	  break;
	}

      case plus:
      case star:
      star_or_plus:
	{
	  iter = rx_mk_r_monop (r_star, *where);
	  if (!iter)
	    {
	      state->err = REG_ESPACE;
	      longjmp (state->err_escape, 1);
	    }
	  iter->observed = *where ? (*where)->observed : 0;
	  if (op_type == plus)
	    {
	      struct rx_exp_node * conc;
	      
	      rx_save_exp (*where);
	      conc = rx_mk_r_binop (r_right_concat, iter, *where);
	      if (!conc)
		{
		  rx_free_exp (iter);
		  state->err = REG_ESPACE;
		  longjmp (state->err_escape, 1);
		}
	      conc->observed = iter->observed;
	      iter = conc;
	    }
	  *where = iter;
	  break;
	}

      case interval:
	{
	  if ((iv == 0) && (iv2 == -1))
	    {
	      op_type = star;
	      goto star_or_plus;
	    }
	  else if ((iv == 1) && (iv2 == -1))
	    {
	      op_type = plus;
	      goto star_or_plus;
	    }

	  iter = rx_mk_r_monop (r_interval, *where);
	  if (!iter)
	    {
	      state->err = REG_ESPACE;
	      longjmp (state->err_escape, 1);
	    }
	  iter->observed = 1;
	  iter->intval = iv;
	  if (iv2 != -1)
	    {
	      iter->intval2 = iv2;
	      *where = iter;
	    }
	  else
	    {
	      struct rx_exp_node * copy_star;
	      struct rx_exp_node * conc;
	      iter->intval2 = iv;

	      rx_save_exp (*where);
	      copy_star = rx_mk_r_monop (r_star, *where);
	      if (!copy_star)
		{
		  rx_free_exp (iter);
		  state->err = REG_ESPACE;
		  longjmp (state->err_escape, 1);
		}
	      copy_star->observed = (*where) && (*where)->observed;
	      conc = rx_mk_r_binop (r_right_concat, iter, copy_star);
	      if (!conc)
		{
		  rx_free_exp (*where);
		  rx_free_exp (copy_star);
		  *where = 0;
		  state->err = REG_ESPACE;
		  longjmp (state->err_escape, 1);
		}
	      conc->observed = 1;
	      *where = conc;
	    }
	  break;
	}
      }
  }
}


enum rx_character_classes
{
  rx_cc_alnum,
  rx_cc_alpha,
  rx_cc_blank,
  rx_cc_cntrl,
  rx_cc_digit,
  rx_cc_graph,
  rx_cc_lower,
  rx_cc_print,
  rx_cc_punct,
  rx_cc_space,
  rx_cc_upper,
  rx_cc_xdigit,
  rx_cc_any
};


struct rx_cc_name
{
  char * name;
  enum rx_character_classes class_id;
};


struct rx_cc_name rx_cc_names[] = 
{
  {"alnum", rx_cc_alnum},
  {"alpha", rx_cc_alpha},
  {"blank", rx_cc_blank},
  {"cntrl", rx_cc_cntrl},
  {"digit", rx_cc_digit},
  {"graph", rx_cc_graph},
  {"lower", rx_cc_lower},
  {"print", rx_cc_print},
  {"punct", rx_cc_punct},
  {"space", rx_cc_space},
  {"upper", rx_cc_upper},
  {"xdigit", rx_cc_xdigit},
  {0, 0}
};


static void
rx_parse_item (struct rx_exp_node ** where,
	       struct rx_parse_state * state,
	       int paren_matched)
{
  int type;
  int iv;
  int token;
  const t_uchar * translate;

  translate = state->translate;

  if (rx_eop (state))
    {
    empty_item:
      *where = 0;
      state->at_beg = 0;
      return;
    }

  /* nested subexpressions */
  if (state->dfa_only && rx_scan_op (state, '('))
    {
      state->at_beg = 1;
      rx_parse_alt (where, state, 1);
      if (!(rx_scan_op (state, ')')))
	{
	  state->err = REG_ELPAREN;
	  longjmp (state->err_escape, 1);
	}
      state->at_beg = 0;
      return;
    }

  if (rx_scan_op (state, '('))
    {
      int exp_number;

      exp_number = state->exp_number;
      ++state->exp_number;
      state->at_beg = 1;
      rx_parse_alt (where, state, 1);
      if (!(rx_scan_op (state, ')')))
	{
	  state->err = REG_ELPAREN;
	  longjmp (state->err_escape, 1);
	}
      state->at_beg = 0;
      if (exp_number < 10)
	state->backrefs_valid[exp_number] = 1;
      {
	struct rx_exp_node * n;
	n = rx_mk_r_monop (r_parens, *where);
	if (!n)
	  {
	    state->err = REG_ESPACE;
	    longjmp (state->err_escape, 1);
	  }
	n->intval = exp_number;
	n->observed = 1;
	*where = n;
      }
      return;
    }

  if (rx_scan (state, "[[:(", 4))
    {
      state->at_beg = 1;
      rx_parse_alt (where, state, 2);
      if (!(rx_scan (state, "):]]", 4)))
	{
	  state->err = REG_ELPAREN;
	  longjmp (state->err_escape, 1);
	}
      state->at_beg = 0;
      {
	struct rx_exp_node * n;
	n = rx_mk_r_monop (r_parens, *where);
	if (!n)
	  {
	    state->err = REG_ESPACE;
	    longjmp (state->err_escape, 1);
	  }
	n->intval = 0;
	n->observed = 0;
	*where = n;
      }
      return;
    }

  if (   ((paren_matched == 1) && rx_scan_op_ahead (state, ')'))
      || ((paren_matched == 2) && rx_scan_ahead (state, "):]]", 4)))
    {
      *where = 0;
      return;
    }

  if (rx_scan (state, "[[:cut ", 7))
    {
      unsigned const char * bound;
      int val;
      int sign;

      state->at_beg = 0;
      sign = 1;
      bound = state->pos + state->size;
      val = 0;

      while (   (state->pos < bound)
	     && char_is_space (translate[*state->pos]))
	++state->pos;

      if (   (state->pos >= bound)
	  || !(   (translate[*state->pos] == '%')
	       || (translate[*state->pos] == '-')
	       || char_is_digit (translate[*state->pos])))
	{
	bad_cut:
	  state->err = REG_BADPAT;
	  longjmp (state->err_escape, 1);
	}

      if (translate[*state->pos] == '%')
	{
	  val = state->cut_count++;
	  ++state->pos;
	}
      else
	{
	  if (translate[*state->pos] == '-')
	    {
	      sign = -1;
	      ++state->pos;
	    }
	  while (   (state->pos < bound)
		 && char_is_digit (translate [*state->pos]))
	    val = val * 10 + (translate [*(state->pos++)] - '0');
	  val = val * sign;
	}

      while (   (state->pos < bound)
	     && char_is_space (translate [*state->pos]))
	++state->pos;

      if (!rx_scan (state, ":]]", 3))
	goto bad_cut;

      *where = rx_mk_r_int (r_cut, val);
      if (!*where)
	{
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      (*where)->observed = 1;
      return;
    }

  /* anchors */
  if (   !state->dfa_only
      && (state->extended_p || state->at_beg)
      && rx_scan (state, "^", 1))
    {
      iv = '^';
    make_context_node:
      type = r_context;
      {
	struct rx_exp_node * n;
	n = rx_mk_r_int (type, iv);
	if (!n)
	  {
	    state->err = REG_ESPACE;
	    longjmp (state->err_escape, 1);
	  }
	*where = n;
	n->observed = 1;
	state->at_beg = (iv == '^');
	return;
      }      
    }
  else if (!state->dfa_only && state->extended_p && !state->at_beg && rx_scan (state, "^", 1))
    {
      state->err = REG_BADPAT;
      longjmp (state->err_escape, 1);
    }

  {
    int at_end;

    at_end = (   !state->dfa_only
	      && !state->extended_p
	      && (   ((paren_matched == 1) && rx_scan_ahead (state, "$\\)", 3))
		  || ((paren_matched == 2) && rx_scan_ahead (state, "$):]]", 5))
		  || rx_scan_ahead (state, "$\\|", 3)
		  || (   rx_scan_ahead (state, "$", 1)
		      && (state->pos + 1 == (state->pattern + state->size)))));

    if (!state->dfa_only && (state->extended_p || at_end) && rx_scan (state, "$", 1))
      {
	type = r_context;
	iv = '$';
	goto make_context_node;
      }
    else if (!state->dfa_only && !state->extended_p && !at_end && rx_scan (state, "$", 1))
      {
	state->err = REG_BADPAT;
	longjmp (state->err_escape, 1);
      }
  }

  /* The characters *, ?, +, and { are sometimes valid,
   * sometimes special, and sometimes an error:
   */
  if (token = '*', rx_scan_op_ahead (state, token))
    {
      iv = '*';
      goto got_iterator;
    }
  if (token = '+', rx_scan_op_ahead (state, token))
    {
      iv = '+';
      goto got_iterator;
    }
  if (!state->dfa_only && (token = '{', rx_scan_op_ahead (state, token)))
    {
      iv = '{';
      goto got_iterator;
    }
  if (token = '?', rx_scan_op_ahead (state, token))
    {
      iv = '?';
    got_iterator:
      if (!state->extended_p && state->at_beg)
	{
	  rx_scan_op (state, token);
	  goto begin_string;
	}
      else
	goto empty_item;
    }

  /* empty before alt */
  if (rx_scan_op_ahead (state, '|'))
    goto empty_item;

  /* csets */
  if (rx_scan (state, ".", 1))
    {
      bits cs;
      struct rx_exp_node * n;
      state->at_beg = 0;
      cs = bits_alloc (rx_nfa_cache_limits (), rx_8bit_bits_tree_rule);
      if (!cs)
	{
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      if (bits_fill (cs) || bits_remove (cs, 0))
	{
	  bits_free (cs);
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      if (state->newline_separates_lines)
	{
	  if (bits_remove (cs, '\n'))
	    {
	      bits_free (cs);
	      state->err = REG_ESPACE;
	      longjmp (state->err_escape, 1);
	    }
	}
      n = rx_mk_r_cset_take (r_cset, state->cset_size, cs);
      if (!n)
	{
	  bits_free (cs);
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      n->observed = 0;
      *where = n;
      return;
    }


  if (rx_scan (state, "[", 1))
    {
      int invert_it;
      bits cs;
      struct rx_exp_node * n;

      state->at_beg = 0;
      invert_it = rx_scan (state, "^", 1);
      
      cs = bits_alloc (rx_nfa_cache_limits (), rx_8bit_bits_tree_rule);
      if (!cs)
	{
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}	

      /* An initial ']' is special. */

      if (rx_scan_ahead (state, "]", 1))
	goto normal_char;

      while (!rx_scan (state, "]", 1))
	{
	  if (rx_eop (state))
	    {
	    short_bracket:
	      bits_free (cs);
	      state->err = REG_EBRACK;
	      longjmp (state->err_escape, 1);
	    }
	  else if (rx_scan (state, "[=", 2))
	    {
	      /* Equivalence classes not supported yet.
	       */
	      bits_free (cs);
	      state->err = REG_ECOLLATE;
	      longjmp (state->err_escape, 1);
	    }
	  else if (rx_scan (state, "[:", 2))
	    {
	      int x;
	      int cl;

	      x = 0;
	      while (rx_cc_names[x].name)
		{
		  if (rx_scan (state, rx_cc_names[x].name, str_length (rx_cc_names[x].name)))
		    {
		      cl = rx_cc_names[x].class_id;
		      break;
		    }
		  else
		    ++x;
		}

	      if (   !rx_cc_names[x].name
		  || !rx_scan (state, ":]", 2))
		{
		  bits_free (cs);
		  state->err = REG_ECTYPE;
		  longjmp (state->err_escape, 1);
		}

	      for (x = 0; x < state->cset_size; ++x)
		{
		  int in;

		  switch (cl)
		    {
		    default:
		      while (1)
			panic ("strange character class");

		    case rx_cc_alnum:
		      in = char_is_alnum_locale (x);
		      break;

		    case rx_cc_alpha:
		      in = char_is_alpha_locale (x);
		      break;

		    case rx_cc_blank:
		      in = ((x == ' ') || (x == '\t'));
		      break;
		      
		    case rx_cc_cntrl:
		      in = char_is_control_locale (x);
		      break;

		    case rx_cc_digit:
		      in = char_is_digit_locale (x);
		      break;

		    case rx_cc_graph:
		      in = char_is_graph_locale (x);
		      break;

		    case rx_cc_lower:
		      in = char_is_lower_locale (x);
		      break;

		    case rx_cc_print:
		      in = char_is_printable_locale (x);
		      break;

		    case rx_cc_punct:
		      in = char_is_punct_locale (x);
		      break;

		    case rx_cc_space:
		      in = char_is_space_locale (x);
		      break;

		    case rx_cc_upper:
		      in = char_is_upper_locale (x);
		      break;

		    case rx_cc_xdigit:
		      in = char_is_xdigit_locale (x);
		      break;
		    }

		  if (in)
		    {
		      bits it;
		      it = rx_inverse_translation (state->n_members, state->cset_size,
						   state->inv_tr,
						   translate, x);
		      if (!it || bits_union (cs, it))
			{
			  bits_free(cs);
			  state->err = REG_ECTYPE;
			  longjmp (state->err_escape, 1);
			}
		    }
		}
	    }
	  else
	    {
	      int first;
	      int last;

	    normal_char:

	      if (!rx_scan (state, "[.", 2))
		{
		  first = translate[*state->pos];
		  ++state->pos;
		}
	      else
		{
		  first = translate[*state->pos];
		  ++state->pos;
		  if (!rx_scan (state, ".]", 2))
		    {
		      /* Illegal collating symbol
		       */
		      bits_free (cs);
		      state->err = REG_ECOLLATE;
		      longjmp (state->err_escape, 1);
		    }
		}

	      {
		bits it;
		it = rx_inverse_translation (state->n_members, state->cset_size,
					     state->inv_tr,
					     translate, first);
		if (!it || bits_union (cs, it))
		  {
		    bits_free (cs);
		    state->err = REG_ESPACE;
		    longjmp (state->err_escape, 1);
		  }
	      }
	      if (   !rx_scan_ahead (state, "-]", 2)
		  && rx_scan (state, "-", 1))
		{
		  if (rx_eop (state))
		    goto short_bracket;
		  if (!rx_scan (state, "[.", 2))
		    {
		      last = translate[*state->pos];
		      ++state->pos;
		    }
		  else
		    {
		      last = translate[*state->pos];
		      ++state->pos;
		      if (!rx_scan (state, ".]", 2))
			{
			  /* Illegal collating symbol
			   */
			  bits_free (cs);
			  state->err = REG_ECOLLATE;
			  longjmp (state->err_escape, 1);
			}
		    }
		  if (first > last)
		    {
		      bits_free (cs);
		      state->err = REG_ERANGE;
		      longjmp (state->err_escape, 1);
		    }
		  {
		    int c;

		    for (c = 0; c < 256; ++c)
		      {
			if (   (char_cmp_locale (first, c) <= 0)
			    && (char_cmp_locale (c, last) <= 0))
			  {
			    bits it;
			    it = rx_inverse_translation (state->n_members, state->cset_size,
							 state->inv_tr,
							 translate, c);
			    if (!it || bits_union (cs, it))
			      {
				bits_free (cs);
				state->err = REG_ESPACE;
				longjmp (state->err_escape, 1);
			      }
			  }
		      }
		  }
		}
	    }
	}

      if (invert_it)
	{
	  if (bits_complement (cs) || (state->newline_separates_lines && bits_remove (cs, '\n')))
	    {
	      bits_free (cs);
	      state->err = REG_ESPACE;
	      longjmp (state->err_escape, 1);
	    }
	}

      n = rx_mk_r_cset_take (r_cset, state->cset_size, cs);
      if (!n)
	{
	  bits_free (cs);
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      *where = n;
      n->observed = 0;
      
      return;
    }

  if (rx_scan (state, "\\", 1))
    {
      if (rx_eop (state))
	{
	  state->err = REG_EESCAPE;
	  longjmp (state->err_escape, 1);
	}

      if (state->dfa_only)
	goto escaped_char_default;
      
      switch (translate[*state->pos])
	{
	default:
	escaped_char_default:
	  iv = translate[*state->pos];
	  ++state->pos;
	  goto begin_string;

	case '0': case '1':  case '2':  case '3':  case '4':
	case '5': case '6':  case '7':  case '8':  case '9':
	  iv = translate[*state->pos];
	  ++state->pos;
	  if (!state->backrefs_valid[iv - '0'])
	    {
	      state->err = REG_ESUBREG;
	      longjmp (state->err_escape, 1);
	    }
	  else
	    goto make_context_node;
	}
    }
  
  /* string */
  iv = translate[*state->pos];
  ++state->pos;

 begin_string:
  {
    bits it;
    state->at_beg = 0;
    it = rx_inverse_translation (state->n_members, state->cset_size,
				 state->inv_tr,
				 translate, iv);
    if (!it)
      {
	state->err = REG_ESPACE;
	longjmp (state->err_escape, 1);
      }
    if (state->n_members[iv] == 0)
      {
	*where = 0;
	return;
      }
    else if (state->n_members[iv] > 1)
      {
	bits cs;
	struct rx_exp_node * match;

	cs = bits_dup (it);
	if (!cs)
	  {
	    state->err = REG_ESPACE;
	    longjmp (state->err_escape, 1);
	  }
	match = rx_mk_r_cset_take (r_cset, state->cset_size, cs);
	if (!match)
	  {
	    bits_free (cs);
	    state->err = REG_ESPACE;
	    longjmp (state->err_escape, 1);
	  }
	match->observed = 0;
	*where = match;
	return;
      }
  }
  {
    char c;
    c = iv;
    *where = rx_mk_r_str (r_string, &c, 1, uni_iso8859_1);
    if (!*where)
      {
	state->err = REG_ESPACE;
	longjmp (state->err_escape, 1);
      }
  }
  (*where)->observed = 0;
  {
    unsigned const char * bound;
    bound = state->pattern + state->size;
    while (state->pos < bound)
      {
	switch (translate[*state->pos])
	  {
	  default:
	  add_simple_char:
	    {
	      bits it;

	      it = rx_inverse_translation (state->n_members, state->cset_size,
					   state->inv_tr,
					   translate, *state->pos);
	      if (!it)
		{
		  state->err = REG_ESPACE;
		  longjmp (state->err_escape, 1);
		}
	      if (state->n_members[*state->pos] != 1)
		return;

	      {
		t_uchar * new_str;
		new_str = ((t_uchar *)
			   rx_nfa_cache_realloc ((*where)->str, (*where)->str_len + 1));
		if (!new_str)
		  {
		    state->err = REG_ESPACE;
		    longjmp (state->err_escape, 1);
		  }
		(*where)->str = new_str;
		(*where)->str[(*where)->str_len] = translate[*state->pos];
		++(*where)->str_len;
	      }

	      ++state->pos;
	      continue;
	    }

	  case '.':
	  case '*':
	  case '[':
	    return;

	  case '{':
	  case '^':
	    if (state->dfa_only)
	      goto add_simple_char;
	    /* fall through */
	  case '(':
	  case '|':
	  case '+':
	  case '?':
	    if (!state->extended_p)
	      goto add_simple_char;
	    else
	      return;

	  case ')':
	    if (   (state->extended_p && (paren_matched == 1))
		|| ((paren_matched == 2) && rx_scan_ahead (state, "):]]", 4)))
	      return;
	    else
	      goto add_simple_char;

	  case '$':
	    if (state->dfa_only)
	      goto add_simple_char;
	    if (   state->extended_p
		|| rx_scan_ahead (state, "$\\|", 3)
		|| ((paren_matched == 1) && rx_scan_ahead (state, "$\\)", 3))
		|| ((paren_matched == 2) && rx_scan_ahead (state, "$):]]", 5))
		|| (   rx_scan_ahead (state, "$", 1)
		    && (state->pos + 1 == bound)))
	      return;
	    else
	      goto add_simple_char;

	  case '\\':
	    if ((state->pos + 1) == bound)
	      {
		state->err = REG_EESCAPE;
		longjmp (state->err_escape, 1);
	      }

	    if (!state->dfa_only && char_is_digit (translate[*(state->pos + 1)]))
	      return;

	    if (state->extended_p)
	      {
		++state->pos;
		goto add_simple_char;
	      }

	    switch (translate[*(state->pos + 1)])
	      {
	      default:
		++state->pos;
		goto add_simple_char;
	      case '{':
		if (state->dfa_only)
		  goto add_simple_char;
		else
		  return;
	      case ')':
		if (!state->extended_p && (paren_matched == 1))
		  return;
		else
		  {
		    ++state->pos;
		    goto add_simple_char;
		  }
	      case '(':
	      case '|':
	      case '+':
	      case '?':
		return;
	      }
	  }
      }
  }
}


/* Regexp Optimizer
 *
 * This function rewrites an expression returned by the parser,
 * to produce an equivalent expression that can be matched more
 * quickly.
 *
 * The gist of the optimization is to move regexp constructs which are 
 * not regular expressions closer to the root of the tree.
 */

static struct rx_exp_node * rx_optimize (struct rx_parse_state * state, struct rx_exp_node * n);

static struct rx_exp_node *
rx_optimize_combination_left (struct rx_parse_state * state,
			      enum rx_exp_node_type type, 
			      struct rx_exp_node * n)
{
  if (!n)
    return 0;

  n = rx_optimize (state, n);

  if (n->type != type)
    return n;

  if (!n->observed)
    return n;

  n->right = rx_optimize_combination_left (state, type, n->right);

  if (!n->right || !n->right->observed)
    return n;

  if (   (n->right->type != type)
      || (n->right->right->observed))
    return n;

  {
    struct rx_exp_node * tmp;

    tmp = n->right->right;
    n->right->right = n->right->left;
    n->right->left = n->left;
    n->left = n->right;
    n->right = tmp;
    n->observed = 1;
    n->left->observed = 1;
    return n;
  }
}


static struct rx_exp_node *
rx_optimize_combination_right (struct rx_parse_state * state,
			       enum rx_exp_node_type type, struct rx_exp_node * n)
{
  if (!n)
    return 0;

  n = rx_optimize (state, n);

  if (!n)
    return 0;

  if (n->type != type)
    return n;

  if (!n->observed)
    return n;

  n->left = rx_optimize_combination_right (state, type, n->left);

  if (!n->left || !n->left->observed)
    return n;

  if (   (n->left->type != type)
      || (n->left->left->observed))
    return n;

  {
    struct rx_exp_node * tmp;

    tmp = n->left->left;
    n->left->left = n->left->right;
    n->left->right = n->right;
    n->right = n->left;
    n->left = tmp;
    n->observed = 1;
    n->right->observed = 1;
    return n;
  }
}


static struct rx_exp_node *
rx_optimize_strings (struct rx_parse_state * state,
		     struct rx_exp_node * r)
{
  if (   ((r->type == r_concat) || (r->type == r_right_concat))
      && (r->left->type == r_string)
      && (r->right->type == r_string))
    {
      struct rx_exp_node * t;
      t_uchar * new_str;
      
      new_str = ((t_uchar *)
		 rx_nfa_cache_realloc (r->left->str, r->left->str_len + r->right->str_len));
      if (!new_str && (r->left->str_len + r->right->str_len))
	{
	  state->err = REG_ESPACE;
	  longjmp (state->err_escape, 1);
	}
      r->left->str = new_str;
      mem_move (r->left->str, r->right->str, r->right->str_len);
      r->left->str_len += r->right->str_len;
      t = r->left;
      r->left = 0;
      r->right = 0;
      rx_free_exp (r);
      r = t;
    }
  return r;
}


static struct rx_exp_node *
rx_optimize (struct rx_parse_state * state, struct rx_exp_node * n)
{
  if (!n)
    return 0;

  if (!n->observed)
    return n;

  if (   (n->type != r_alternate)
      && (n->type != r_concat))
    {
      if (n->left)
	n->left = rx_optimize (state, n->left);

      /* r_right_concat not optimized for now.
       */
      if (n->right)
	n->right = rx_optimize (state, n->right);
      return n;
    }

  {
    struct rx_exp_node * l;
    struct rx_exp_node * r;
    int l_raisable;
    int r_raisable;

    l = rx_optimize_combination_left (state, n->type, n->left);
    r = rx_optimize_combination_right (state, n->type, n->right);

    if (l)
      l_raisable = (   (l->type == n->type)
		    && l->right
		    && (!l->right->observed));
    else
      l_raisable = 0;

    if (r)
      r_raisable = (   (r->type == n->type)
		    && r->left
		    && (!r->left->observed));
    else
      r_raisable = 0;

    if (l && !l->observed && r_raisable)
      {
	struct rx_exp_node * tmp;
	tmp = r->right;
	r->right = r->left;
	r->left = l;
	r = rx_optimize_strings (state, r);
	n->right = tmp;
	n->left = r;
	r->observed = 0;
	n->observed = 1;
	return n;
      }
    else if (r && !r->observed && l_raisable)
      {
	struct rx_exp_node * tmp;
	tmp = l->left;
	l->left = l->right;
	l->right = r;
	n->left = tmp;
	n->right = l;
	l->observed = 0;
	n->observed = 1;
	return n;
      }
    else if (l_raisable && r_raisable)
      {
	struct rx_exp_node * leafs[4];
	leafs[0] = l->left;
	leafs[1] = l->right;
	leafs[2] = r->left;
	leafs[3] = r->right;

	n->left = leafs[0];
	n->right = l;
	l->left = r;
	r->left = leafs[1];
	r->right = leafs[2];
	l->right = leafs[3];

	n->observed = 1;
	l->observed = 1;
	r->observed = 0;
	return n;
      }
    else
      {
	n->left = l;
	n->right = r;
	return n;
      }
  }
}


/*(c rx_parse)
 * int rx_parse (struct rx_exp_node ** rx_exp_p,
 *               int * nsub,
 *               const char *pattern,
 *               int size,
 *               int extended_p,
 *               int newline_separates_lines,
 *               int dfa_only,
 *               int cset_size,
 *               t_uchar *translate);
 * 
 * `rx_parse' translates a string into a regexp expression tree.  The
 * syntax of a regexp is introduced in xref:"An Introduction to Posix
 * Regexps" and defined formally in xref:"Describing Regexps
 * Formally".
 *
 * Return 0 upon success, and an error code upon failure.
 * See xref:"Rx Error Codes".
 * 
 * `*rx_exp_p' returns the compiled expression.
 * 
 * `*nsub' returns the number of parenthesized subexpressions with
 * a non-0 subexpression number.
 * 
 * `pattern' is the regexp string to be compiled.
 * 
 * `size' is the length of that string.
 * 
 * If `extended_p' is 0, then use "Basic Regexp Expression" syntax,
 * otherwise use "Extended Regular Expression" syntax.  
 *
 * `newline_separates_lines' controls whether or not the input string
 * (the string being compared to the pattern) is treated as one line
 * of text, or as any number of lines each ending with a newline
 * ({'\n'}) character.  If `newline_separates_lines' is not 0, then
 * the pattern dot ({'.'})  and complementary character sets such as
 * `[^a]' do not match newline characters (so that what they match is
 * never more than one line of text).  For consistency, when compiling
 * with `newline_separates_lines' the anchor expression `^' should
 * match immediately after a newline and the anchor expression `$'
 * should match immediately before a newline -- but the rules about
 * anchors don't effect compilation.  If `newline_separates_lines' is
 * 0, then {'.'}  and complementary character sets do include newline
 * and anchors should match only at the beginning and end of a string.
 *
 * If `dfa_only' is not 0, compile pure regular expressions, not
 * regexps.  This means that operators which are not valid in regular
 * expressions are interpreted as ordinary characters.  Those
 * operators are: `re{x,y}' (intervals), `^re' and `re$' (anchors),
 * and `\<n>' (backreferences).
 *
 * `cset_size' is the size of the character set (usually 256).
 * 
 * `translate' is an array of `cset_size' characters, defining a
 * mapping from characters to characters.  The compiler reads
 * `pattern' through this mapping (i.e., parses according to
 * `translate[*pattern]' instead of simply `*pattern').  Additionally,
 * the parsed pattern is modified to achieve the effect of translating
 * a target string (during a match) through the same translation.
 * Conceptually, the functions that perform matching could translate
 * the input string being compared to the pattern by using something
 * like `translate[string[x]]' but in fact, the same effect is
 * achieved by specially compiling the pattern.  If `0' is passed for
 * `translate', the effect is the same as passing an identity table in
 * which `translate[c] == c' for all characters.
 */
int
rx_parse (struct rx_exp_node ** rx_exp_p,
	  int * nsub,
	  const t_uchar * pattern,
	  size_t size,
	  int extended_p,
	  int newline_separates_lines,
	  int dfa_only,
	  int cset_size,
	  const t_uchar * translate)
{
  struct rx_parse_state state;

  mem_set ((t_uchar *)&state, 0, sizeof (state));

  state.pos = pattern;
  state.pattern = pattern;
  state.size = size;
  state.extended_p = extended_p;
  state.newline_separates_lines = newline_separates_lines;
  state.cset_size = cset_size;
  if (!translate)
    translate = rx_id_translation_table;
  state.translate = translate;
  state.at_beg = 1;
  state.exp_number = 1;
  state.dfa_only = dfa_only;
  state.cut_count = 1;

  if (setjmp (state.err_escape))
    {
      rx_free_exp (state.root);
      cleanup_parse_state (&state);
      return state.err;
    }
  else
    {
      rx_parse_alt (&state.root, &state, 0);
      *nsub = state.exp_number;
      if (rx_scan_op_ahead (&state, ')'))
	{
	  state.err = REG_ERPAREN;
	  longjmp (state.err_escape, 1);
	}

      if (!rx_eop (&state))
	{
	  state.err = REG_BADPAT;
	  longjmp (state.err_escape, 1);
	}

      *rx_exp_p = rx_optimize (&state, state.root);
      /* *rx_exp_p = state.root; */
      cleanup_parse_state (&state);
      return 0;
    }
}



/* Predefined Translation Tables
 */

/*(c rx_id_translation_table :category variable)
 * t_uchar rx_id_translation_table[256];
 * 
 * An identity mapping of the entire character set.
 * That is:
 *
 *	rx_id_translation_table[N] == N
 *
 * See xref:"rx_parse".
 */
const t_uchar rx_id_translation_table[256] =
{
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,

 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
 120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
 190, 191, 192, 193, 194, 195, 196, 197, 198, 199,

 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
 210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
 220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
 240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
 250, 251, 252, 253, 254, 255
};


/*(c rx_case_fold_translation_table :category variable)
 * t_uchar rx_case_fold_translation_table[256];
 * 
 * An identity mapping of the entire character set.
 * That is:
 *
 *	rx_id_translation_table[N] == N
 *
 * except for `N' which are upper-case letters.
 * For those:
 *
 *	rx_id_translation_table[N] == char_to_lower (N)
 *
 * See xref:"rx_parse".
 */
const t_uchar rx_case_fold_translation_table[256] =
{
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
 60, 61, 62, 63, 64, 97, 98, 99, 100, 101,
 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
 122, 91, 92, 93, 94, 95, 96, 97, 98, 99,

 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
 120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
 160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
 180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
 190, 191, 192, 193, 194, 195, 196, 197, 198, 199,

 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
 210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
 220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
 240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
 250, 251, 252, 253, 254, 255
};

