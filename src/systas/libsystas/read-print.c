/* read-print.c - reading s-expressions
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


#include <stddef.h>
#include <ctype.h>
#include <fcntl.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "hackerlab/bugs/panic.h"
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/char/str.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/vfdbuf.h"
#include "hackerlab/char/char-name.h"
#include "systas/libsystas/weaks.h"
#include "systas/libsystas/procs.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/scheme.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/vuprocs.h"
#include "systas/libsystas/kw.h"


/************************************************************************
 *(h0 "Reading and Writing Data Structures")
 *
 * These functions are used to read or write Scheme data structures.
 * Every data structure has a printed representation, but not every data
 * structure can be read back in.  For example, an structure might be
 * printed as:
 * 
 * 	#<structure 80ddf20>
 * 
 * but trying to read that syntax will cause an error.
 * 
 * Lists, numbers, strings, and vectors are examples of data types that
 * can be both read and written.  Documentation for each type explains the
 * read syntax of that type, if it has one.
 */




static SCM scm_lreadparen (SCM * tok_buf, SCM port, SCM name, int case_i, SCM sharp, SCM this_is, SCM read_params);
static SCM scm_read_string_list (SCM * tok_buf,
				 int indent_width,
				 SCM port,
				 int case_i,
				 SCM sharp,
				 SCM this_is,
				 SCM read_params,
				 int carrot_terminates);

static SCM s_read;
static SCM s_write;
static SCM s_display;

SCM_SYMBOL (s_port_not_buffered, "port-not-buffered");
SCM_SYMBOL (s_bogus_special_symbol, "bad-special-symbol-syntax");
SCM_SYMBOL (s_bogus_string_list, "bad-string-list-syntax");
SCM_SYMBOL (s_premature_eof, "premature-eof");
SCM_SYMBOL (s_cyclic_disabled, "cyclic references not enabled");
SCM_SYMBOL (s_string, "string");
SCM_SYMBOL (s_list, "list");
SCM_SYMBOL (s_vector, "vector");
SCM_SYMBOL (s_procedure_print_name, "procedure-print-name");
SCM_SYMBOL (s_cycles_cycles1, ":cycles contradicts :cycles1");
SCM_SYMBOL (s_cyclic_reference_number, "corrupted cyclic reference number");
SCM_SYMBOL (s_not_reading_cycles, "cyclic reference (\"##n\") detected while not reading cycles");
SCM_SYMBOL (s_bad_cyclic, "ill-formed cyclic reference");
SCM_SYMBOL (s_undefined_cyclic, "undefined cyclic reference");
SCM_SYMBOL (s_multiply_defined_cyclic, "multiply defined cyclic reference");
SCM_SYMBOL (s_cyclic_immediate, "illegal use of #N=");

SCM_KEYWORD (kw_like, "like");
SCM_KEYWORD (kw_strict, "strict");
SCM_KEYWORD (kw_cycles, "cycles");
SCM_KEYWORD (kw_cycles1, "cycles1");

SCM_PROCEDURE (proc_read, "read");
SCM_PROCEDURE (proc_write, "write");



/* SCM_WHITE_SPACES are other characters which should be treated like spaces
 * in programs, excluding newline characters.
 */
#define SCM_WHITE_SPACES  		' ':case '\r':case '\f': case '\t'


/* SCM_UNPRINTABLE are characters which, in strings, should be printed 
 * using an escape code.
 */
#define SCM_UNPRINTABLE  '\00':  case '\01':  case '\02':  case '\03':  \
		    case '\04':  case '\05':  case '\06':  case '\07':  \
		    case '\010':  case '\011':  case '\012':  case '\013':  \
		    case '\014':  case '\015':  case '\016':  case '\017':  \
		    case '\020':  case '\021':  case '\022':  case '\023':  \
		    case '\024':  case '\025':  case '\026':  case '\027':  \
		    case '\030':  case '\031':  case '\032':  case '\033':  \
		    case '\034':  case '\035':  case '\036':  case '\037': \
		    case '\177':  \
		    case '\200':  case '\201':  case '\202':  case '\203':  \
		    case '\204':  case '\205':  case '\206':  case '\207':  \
		    case '\210':  case '\211':  case '\212':  case '\213':  \
		    case '\214':  case '\215':  case '\216':  case '\217':  \
		    case '\220':  case '\221':  case '\222':  case '\223':  \
		    case '\224':  case '\225':  case '\226':  case '\227':  \
		    case '\230':  case '\231':  case '\232':  case '\233':  \
		    case '\234':  case '\235':  case '\236':  case '\237':  \
		    case '\240':  case '\241':  case '\242':  case '\243':  \
		    case '\244':  case '\245':  case '\246':  case '\247':  \
		    case '\250':  case '\251':  case '\252':  case '\253':  \
		    case '\254':  case '\255':  case '\256':  case '\257':  \
		    case '\260':  case '\261':  case '\262':  case '\263':  \
		    case '\264':  case '\265':  case '\266':  case '\267':  \
		    case '\270':  case '\271':  case '\272':  case '\273':  \
		    case '\274':  case '\275':  case '\276':  case '\277':  \
		    case '\300':  case '\301':  case '\302':  case '\303':  \
		    case '\304':  case '\305':  case '\306':  case '\307':  \
		    case '\310':  case '\311':  case '\312':  case '\313':  \
		    case '\314':  case '\315':  case '\316':  case '\317':  \
		    case '\320':  case '\321':  case '\322':  case '\323':  \
		    case '\324':  case '\325':  case '\326':  case '\327':  \
		    case '\330':  case '\331':  case '\332':  case '\333':  \
		    case '\334':  case '\335':  case '\336':  case '\337':  \
		    case '\340':  case '\341':  case '\342':  case '\343':  \
		    case '\344':  case '\345':  case '\346':  case '\347':  \
		    case '\350':  case '\351':  case '\352':  case '\353':  \
		    case '\354':  case '\355':  case '\356':  case '\357':  \
		    case '\360':  case '\361':  case '\362':  case '\363':  \
		    case '\364':  case '\365':  case '\366':  case '\367':  \
		    case '\370':  case '\371':  case '\372':  case '\373':  \
		    case '\374':  case '\375':  case '\376':  case '\377'



/* struct cycle_record
 *
 * For every descriptor to which we are writing (by `write' or `display')
 * we keep one of these structures.
 *
 * strict	- If not 0, then use back-reference notation when
 *		  printing _any_ non-immediate object for the second
 *		  time.  Otherwise, only use back-reference notation
 *		  for pairs and vectors.
 *
 * pass		- 1 if this is the first pass of printing, 2 if the
 *		  this is the second pass.  Multi-pass printing is used
 *		  only if the keyword `:cycles' was passed to `write'
 *		  or `display'.
 *
 * obj_number	- If printing with `:cycles' or `:cycles1', this counts
 *		  the number of non-immediate (`:strict'), or pairs and vectors
 *		  (not `:strict') printed.  This number is used for 
 *		  back-reference notation.
 *
 * table	- If printing with cycles, this is a hash table whose keys
 *		  are the back-referencable objects that have been printed
 *		  so far, and whose values are the object numbers of those
 *		  objects.
 *
 * twice	- If printing with cycles, this is a hash table whose keys
 *		  are the back-referencable objects that have been printed
 *		  more than once, and whose values are the object numbers
 *		  of those objects.
 *
 * For GC protection, the tables are made coextensive with the port
 * we are writing to.  (!!! This is a bug -- GC protection should end when
 * the print completes.)
 *
 * During `:cycles1' printing, objects are recorded in `table'.  If an object
 * is seen more than once, a back-reference is printed.
 *
 * `:cycles' printing essentially performs one pass of `:cycles1' printing,
 * discarding the output.  During this pass, if an object is seen more than
 * once, it is recorded in `twice'.  During the second pass, printing produces
 * output; the first time an object in `twice' is printed, it is labeled
 * with forward-reference notation.
 *
 * The keyword `:strict' determines which objects are recorded in the tables.
 * If `:strict' is provided, all non-immediate objects are recorded.  Otherwise,
 * only pairs and vectors are recorded.
 */
struct cycle_record
{
  int strict;
  int pass;
  long obj_number;
  SCM table;
  SCM twice;
};


/* struct read_cycle_record
 *
 * For every descriptor to which we are reading (by `read')
 * we keep one of these structures.
 *
 * table	- A hash table.  Keys are objects that were prefixed by 
 *		  forward-reference notation.  Values are object numbers.
 *		  This is used to resolve back-references.
 *
 * For GC protection, the tables are made coextensive with the port
 * we are reading from.  (!!! This is a bug -- GC protection should end when
 * the read completes.)
 */
struct read_cycle_record
{
  SCM table;
};


/* cycle_records
 *
 * A dynamic array (`ar_ref'), indexed by descriptor numbers,
 * of printer state.
 *
 * Declared `struct cycle_record **' instead of `struct cycle_record
 * *' so that `scm_swap_read_records' can work.
 */
static struct cycle_record ** cycle_records = 0;


/* read_cycle_records
 *
 * A dynamic array (`ar_ref'), indexed by descriptor numbers,
 * of printer state.
 * 
 * Declared `struct read_cycle_record **' instead of `struct
 * read_cycle_record *' so that `scm_swap_read_records' can work.
 */
static struct read_cycle_record ** read_cycle_records = 0;


static struct cycle_record *
cycle_record_ar_ref (int fd)
{
  SCM_INTS_DISABLED;
  struct cycle_record ** where;

  where = (struct cycle_record **)ar_ref ((void **)&cycle_records, lim_use_must_malloc, fd, sizeof (struct cycle_record *));
  if (!*where)
    {
      *where = (struct cycle_record *)scm_must_malloc (sizeof (struct cycle_record));
      mem_set0 ((t_uchar *)*where, sizeof (struct cycle_record));
    }
  return *where;
}

static struct read_cycle_record *
read_cycle_record_ar_ref (int fd)
{
  SCM_INTS_DISABLED;
  struct read_cycle_record ** where;

  where = (struct read_cycle_record **)ar_ref ((void **)&read_cycle_records, lim_use_must_malloc, fd, sizeof (struct read_cycle_record *));
  if (!*where)
    {
      *where = (struct read_cycle_record *)scm_must_malloc (sizeof (struct read_cycle_record));
      mem_set0 ((t_uchar *)*where, sizeof (struct read_cycle_record));
    }
  return *where;
}

/* scm_swap_read_records
 * 
 * See `scm_sys_move_fd'
 *
 */
void
scm_swap_read_records (int fd0, int fd1)
{
  SCM_INTS_DISABLED;
  struct read_cycle_record * r0;
  struct read_cycle_record * r1;
  struct cycle_record * c0;
  struct cycle_record * c1;

  r0 = read_cycle_record_ar_ref (fd0);
  r1 = read_cycle_record_ar_ref (fd1);
  read_cycle_records[fd0] = r1;
  read_cycle_records[fd1] = r0;

  c0 = cycle_record_ar_ref (fd0);
  c1 = cycle_record_ar_ref (fd1);
  cycle_records[fd0] = c1;
  cycle_records[fd1] = c0;
}



/* static char * scm_grow_tok_buf (SCM * tok_buf);
 * 
 * Copy the contents of a string to a larger string.
 *
 * Return a pointer to the character data of the new string.
 * Return the new string itself in `tok_buf'.
 */
static char *
scm_grow_tok_buf (SCM * tok_buf)
{
  SCM_INTS_DISABLED;
  SCM old_tok;
  SCM new_tok;
  size_t len;
  size_t old_len;
  char * from;
  char * to;

  old_tok = *tok_buf;
  old_len = SCM_LENGTH (*tok_buf);
  len = (old_len ? 2 * old_len : 16);
  new_tok = scm_makstr (len);
  from = SCM_STRING_CHARS (*tok_buf);
  to = SCM_STRING_CHARS (new_tok);
  mem_move (to, from, old_len);
  *tok_buf = scm_return_first (new_tok, old_tok);
  return to;
}


/* static int scm_skip_ws (SCM port, int eoferr, SCM read_params);
 * 
 * Read characters until a non-whitespace character is encountered,
 * skipping comments.  Return the first non-whitespace, non-comment 
 * character.
 *
 * port		- the port from which to read characters.
 *
 * eoferr	- If 0, return -1 if EOF is reached.  Otherwise,
 *		  signal an error if EOF is reached.
 *
 * read_params	- Extra arguments to pass to the exception raised 
 *		  if an I/O error occurs.  The exception thrown is:
 *
 *		    (apply throw <errno-symbol> read read_params)
 */
static int 
scm_skip_ws (SCM port, int eoferr, SCM read_params)
{
  SCM_INTS_ENABLED;
  int c;
  int errn;

  while (1)
    {
      SCM_DEFER_INTS;
      c = scm_port_getc (&errn, port);
      SCM_ALLOW_INTS;
      switch (c)
	{
	case -1:
	read_error:
	  if (errn)
	    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	  else if (eoferr)
	    scm_throw (s_premature_eof, scm_cons (port, SCM_EOL));
	  else
	    return c;

	case ';':
	skip_comment:
	  SCM_DEFER_INTS;
	  c = scm_port_getc (&errn, port);
	  SCM_ALLOW_INTS;
	  switch (c)
	    {
	    case -1:
	      goto read_error;
	    default:
	      goto skip_comment;
	    case '\n':
	      break;
	    }
	  break;
	  
	case SCM_WHITE_SPACES:
	case '\n':
	  break;

	default:
	  return c;
	}
    }
}


/* static size_t scm_read_token (int ic,
 *				 SCM * tok_buf,
 *				 SCM port,
 *				 int case_i,
 *				 SCM read_params);
 * 
 * Read an ordinary scheme symbol name, storing it in `tok_buf'.
 * `ic' is the first character, which has already been read.  
 * If `case_i' is not 0, convert the name to lower case as it is read.
 *
 * This may replace `tok_buf' with a larger string.
 */
static size_t 
scm_read_token (int ic, SCM * tok_buf, SCM port, int case_i, SCM read_params)
{
  SCM_INTS_ENABLED;
  int c;
  char *p;
  size_t j;
  int rv;
  int errn;

  c = (case_i ? tolower(ic) : ic);
  p = SCM_STRING_CHARS (*tok_buf);
  j = 0;
  if (!SCM_LENGTH (*tok_buf))
    p = scm_grow_tok_buf (tok_buf);
  p[j] = c;
  ++j;

  while (1)
    {
      while (j + 1 >= SCM_LENGTH (*tok_buf))
	p = scm_grow_tok_buf (tok_buf);
      SCM_DEFER_INTS;
      c = scm_port_getc (&errn, port);
      SCM_ALLOW_INTS;
      switch (c)
	{
	case '(':
	case ')':
	case '"':
	case ';':
	case SCM_WHITE_SPACES:
	case '\n':
	  SCM_DEFER_INTS;
	  rv = scm_port_ungetc (&errn, port, c);
	  SCM_ALLOW_INTS;
	  if (rv < 0)
	    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	  return j;

	case -1:
	  if (errn)
	    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	  return j;

	default:
	  {
	    c = (case_i ? tolower(c) : c);
	    p[j] = c;
	    ++j;
	  }

	}
    }
}


/* static int scm_read_string (int * errn, SCM * tok_buf, SCM port);
 *
 * Read a string. 
 *
 * The initial '"' has already been read.
 *
 * On success return the length of the string (which is a prefix of `*tok_buf'.
 *
 * On EOF, set `*errn' to 0 and return -1.
 *
 * On an I/O error, set `*errn' to an error code and return -1.
 */
static int
scm_read_string (int * errn, SCM * tok_buf, SCM port)
{
  int j;
  int c;

  j = 0;
  while (1)
    {
      SCM_DEFER_INTS;
      c = scm_port_getc (errn, port);
      SCM_ALLOW_INTS;
	  
      if ('"' == c)
	break;

      if (c < 0)
	return -1;
	      
      while (j + 1 >= SCM_LENGTH (*tok_buf))
	scm_grow_tok_buf (tok_buf);

      if (c == '\\')
	{
	  SCM_DEFER_INTS;
	  c = scm_port_getc (errn, port);
	  SCM_ALLOW_INTS;
	  if (c < 0)
	    return -1;
	  switch (c)
	    {
	    case '\n':
	      continue;
	    case '0':
	      c = '\0';
	      break;
	    case 'f':
	      c = '\f';
	      break;
	    case 'n':
	      c = '\n';
	      break;
	    case 'r':
	      c = '\r';
	      break;
	    case 't':
	      c = '\t';
	      break;
	    case 'a':
	      c = '\007';
	      break;
	    case 'v':
	      c = '\v';
	      break;
	    }
	}

      SCM_STRING_CHARS (*tok_buf)[j] = c;
      ++j;
    }
  return j;
}

static void
add_char_to_string (int * str_len, SCM * tok_buf, int c)
{
  while (*str_len + 1 >= SCM_LENGTH (*tok_buf))
    scm_grow_tok_buf (tok_buf);
  SCM_STRING_CHARS (*tok_buf)[*str_len] = c;
  ++*str_len;
}

static int
read_fn_getc (SCM port, SCM read_params)
{
  int errn;
  int c;

  SCM_DEFER_INTS;
  c = scm_port_getc (&errn, port);
  SCM_ALLOW_INTS;
  if (c < 0)
    {
      if (!errn)
	scm_throw (s_premature_eof, scm_cons (proc_read, read_params));
      else
	scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
    }
  return c;
}


void
read_fn_ungetstr_n (t_uchar * str, int len, SCM port, SCM read_params)
{
  int errn;
  int stat;

  SCM_DEFER_INTS;
  stat = vfdbuf_return (&errn, SCM_FD (port), str, len);
  SCM_ALLOW_INTS;
  if (stat < 0)
    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
}


void
read_fn_read_to_column (int indent_width, SCM port, SCM read_params)
{
  int col;
  int c;

  col = 0;
  while (col < indent_width)
    {
      c = read_fn_getc (port, read_params);
      switch (c)
	{
	case ' ':
	  col += 1;
	  goto check_indent;

	case '\t':
	  col += 8;
	  col /= 8;
	  col *= 8;

	check_indent:
	  {
	    static char eight_spaces[9] = "        ";
	    int over;

	    if (col < indent_width)
	      continue;
	    over = col - indent_width;
	    if (over)
	      read_fn_ungetstr_n (eight_spaces + 8 - over, over, port, read_params);
	    return;
	  }
		    
	default:
	  {
	    t_uchar chr;
	    chr = c;
	    read_fn_ungetstr_n (&chr, 1, port, read_params);
	  }
	  return;
	}
    }
}

/* scm_lreadr
 * 
 * Read one value and return.
 *
 * tok_buf is a string to use (and possibly replace) to store
 * a symbol name.
 *
 * case_i means to convert symbol names to lower case.
 *
 * sharp is #f or a procedure to use to read values that begin
 * with "#" if the character following the "#" has no built-in 
 * meaning.
 */
static SCM 
scm_lreadr (SCM * tok_buf, SCM port, int case_i, SCM sharp, SCM this_is, SCM read_params)
{
  SCM_INTS_ENABLED;
  int c;
  size_t j;
  SCM p;
  struct read_cycle_record * cr;
  int errn;
  
  SCM_DEFER_INTS;
  cr = read_cycle_record_ar_ref (SCM_FD (port));
  SCM_ALLOW_INTS;

tryagain:
  c = scm_skip_ws (port, 0, read_params);
  switch (c)
    {
    case -1:
      if (this_is != SCM_BOOL_F)
	scm_throw (s_premature_eof, scm_cons (port, SCM_EOL));
      return SCM_EOF_VAL;

    case '(':
      return scm_lreadparen (tok_buf, port, s_list, case_i, sharp, this_is, read_params);

    case ')':
      scm_wta (SCM_UNDEFINED, scm_unexpected_rparen, s_read);
      goto tryagain;
    
    case '\'':
      {
	SCM q;
	SCM head;
	q = scm_i_quote;
	if (0)
	  {
	  strong_quote:
	    q = scm_i_strong_quote;
	  }
	head = scm_cons (q, SCM_EOL);
	if (this_is != SCM_BOOL_F)
	  scm_hashq_set_x (cr->table, this_is, head);
	SCM_CDR (head) = scm_cons (scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params), SCM_EOL);
	return head;
      }

    case '`':
      {
	SCM head;
	if (1)
	  head = scm_cons (scm_i_quasiquote, SCM_EOL);
	else
	  {
	  strong_quasiquote:
	    head = scm_cons (scm_i_strong_quasiquote, SCM_EOL);
	  }
	if (this_is != SCM_BOOL_F)
	  scm_hashq_set_x (cr->table, this_is, head);
	SCM_CDR (head) = scm_cons (scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params), SCM_EOL);
	return head;
      }

    case ',':
      SCM_DEFER_INTS;
      c = scm_port_getc (&errn, port);
      SCM_ALLOW_INTS;
      if (c == -1)
	{
	  if (errn)
	    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	  p = scm_i_unquote;
	}
      else if ('@' == c)
	p = scm_i_uq_splicing;
      else
	{
	  int rv;
	  SCM_DEFER_INTS;
	  rv = scm_port_ungetc (&errn, port, c);
	  SCM_ALLOW_INTS;
	  if (rv < 0)
	    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	  p = scm_i_unquote;
	}

      {
	SCM head;
	head = scm_cons (p, SCM_EOL);
	if (this_is != SCM_BOOL_F)
	  scm_hashq_set_x (cr->table, this_is, head);
	SCM_CDR (head) = scm_cons (scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params), SCM_EOL);
	return head;
      }

    case '#':
      SCM_DEFER_INTS;
      c = scm_port_getc (&errn, port);
      SCM_ALLOW_INTS;
      switch (c)
	{
	case -1:
	  {
	  read_error:
	    if (errn)
	      scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	    else
	      scm_throw (s_premature_eof, scm_cons (proc_read, read_params));
	  }
	case '\'':
	  {
	    goto strong_quote;
	  }
	case '`':
	  {
	    goto strong_quasiquote;
	  }
	case ':':
	  /* A string list. */
	  {
	    SCM list;
	    list = scm_read_string_list (tok_buf,
					 0,
					 port,
					 case_i,
					 sharp,
					 this_is,
					 read_params,
					 0);
	    if (   (':' != read_fn_getc (port, read_params))
		|| ('#' != read_fn_getc (port, read_params)))
	      scm_throw (s_bogus_string_list, scm_cons (proc_read, read_params));
	    return list;
	  }

	case '|':
	  /* A string list. */
	  {
	    SCM first;
	    SCM list;
	    first = scm_lreadr  (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
	    {
	      t_uchar x;
	      x = read_fn_getc (port, read_params);
	      if (x != ';')
		read_fn_ungetstr_n (&x, 1, port, read_params);
	    }
	    list = scm_read_string_list (tok_buf,
						 0,
						 port,
						 case_i,
						 sharp,
						 this_is,
						 read_params,
						 0);
	    if (   ('|' != read_fn_getc (port, read_params))
		|| ('#' != read_fn_getc (port, read_params)))
	      scm_throw (s_bogus_string_list, scm_cons (proc_read, read_params));
	    return scm_cons (first, list);
	  }

	case '/':
	  {
	    SCM first;
	    SCM list;
	    int indent;

	    first = SCM_UNDEFINED;
	    while (1)
	      {
		c = read_fn_getc (port, read_params);
		switch (c)
		  {
		  case ' ':
		  case '\t':
		    continue;
		  case '\n':
		    goto count_indent;
		  default:
		    {
		      t_uchar chr;
		      chr = c;
		      read_fn_ungetstr_n (&chr, 1, port, read_params);
		    }
		    first = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
		    {
		      t_uchar x;
		      x = read_fn_getc (port, read_params);
		      if (x != ';')
			read_fn_ungetstr_n (&x, 1, port, read_params);
		    }
		    while (1)
		      {
			c = read_fn_getc (port, read_params);
			switch (c)
			  {
			  case ' ':
			  case '\t':
			    continue;
			  case '\n':
			    goto count_indent;
			  default:
			    scm_throw (s_bogus_string_list, scm_cons (proc_read, read_params));
			  }
		      }
		  }
	      }
	  count_indent:
	    indent = 0;
	    while (1)
	      {
		c = read_fn_getc (port, read_params);
		switch (c)
		  {
		  case ' ':
		    indent += 1;
		    continue;

		  case '\t':
		    indent += 8;
		    indent /= 8;
		    indent *= 8;
		    continue;

		  case '/':
		    while (1)
		      {
			c = read_fn_getc (port, read_params);
			switch (c)
			  {
			  case ' ':
			  case '\t':
			    continue;
			  case '\n':
			    read_fn_read_to_column (indent, port, read_params);
			    goto get_indented_string_list;
			  default:
			    scm_throw (s_bogus_string_list, scm_cons (proc_read, read_params));
			  }
		      }

		  default:
		    scm_throw (s_bogus_string_list, scm_cons (proc_read, read_params));
		  }
	      }

	  get_indented_string_list:
	    list = scm_read_string_list (tok_buf,
					 indent,
					 port,
					 case_i,
					 sharp,
					 this_is,
					 read_params,
					 0);
	    if (   ('/' != read_fn_getc (port, read_params))
		|| ('#' != read_fn_getc (port, read_params)))
	      scm_throw (s_bogus_string_list, scm_cons (proc_read, read_params));
	    if (first != SCM_UNDEFINED)
	      return scm_cons (first, list);
	    else
	      return list;
	  }


	case '#':
	  {
	    SCM next_is;
	    long n;

	    if (!cr->table)
	      scm_throw (s_cyclic_disabled, SCM_EOL);
	    n = 0;
	    SCM_DEFER_INTS;
	    c = scm_port_getc (&errn, port);
	    SCM_ALLOW_INTS;
	    if (c == -1)
	      goto read_error;
	    if (!isdigit (c))
	      scm_throw (s_bad_cyclic, scm_cons (scm_int_to_char (c), SCM_EOL));
	    while (isdigit (c))
	      {
		n = n * 10 + c - '0';
		SCM_DEFER_INTS;
		c = scm_port_getc (&errn, port);
		SCM_ALLOW_INTS;
		if ((c == -1) && errn)
		  goto read_error;
	      }
	    next_is = scm_long2num (n);
	    if (c != '=')
	      {
		int rv;
		SCM_DEFER_INTS;
		rv = scm_port_ungetc (&errn, port, c);
		SCM_ALLOW_INTS;
		if (rv < 0)
		  scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
		p = scm_hashq_ref (cr->table, next_is, SCM_BOOL_F);
		if (p == SCM_BOOL_F)
		  scm_throw (s_undefined_cyclic, scm_cons (next_is, SCM_EOL));
		return p;
	      }
	    else
	      {
		SCM it;
		p = scm_hashq_ref (cr->table, next_is, SCM_BOOL_F);
		if (p != SCM_BOOL_F)
		  scm_throw (s_multiply_defined_cyclic, scm_cons (next_is, SCM_EOL));
		it = scm_lreadr (tok_buf, port, case_i, sharp, next_is, read_params);
		return it;
	      }
	  }
	case '(':
	  {
	    SCM head;
	    SCM elts;
	    int n;
	    int rv;

	    head = scm_makvector (0, SCM_EOL, 0, SCM_BOOL_F);
	    if (this_is != SCM_BOOL_F)
	      scm_hashq_set_x (cr->table, this_is, head);
	    rv = scm_port_ungetc (&errn, port, c);
	    if (rv < 0)
	      scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	    elts = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
	    n = scm_ilength (elts);
	    if (n < 0)
	      scm_wta (SCM_UNDEFINED, scm_bad_vector_syntax, s_read);
	    if (!n)
	      {
		return head;
	      }
	    else
	      {
		scm_makvector (n, elts, 1, head);
		return head;
	      }
	  }

	case 't':
	case 'T':
	  if (this_is != SCM_BOOL_F)
	    scm_wta (SCM_UNDEFINED, s_cyclic_immediate, s_read);
	  return SCM_BOOL_T;
	case 'f':
	case 'F':
	  if (this_is != SCM_BOOL_F)
	    scm_wta (SCM_UNDEFINED, s_cyclic_immediate, s_read);
	  return SCM_BOOL_F;

	case 'b':
	case 'B':
	case 'o':
	case 'O':
	case 'd':
	case 'D':
	case 'x':
	case 'X':
	case 'i':
	case 'I':
	case 'e':
	case 'E':
	  {
	    int rv;
	    SCM_DEFER_INTS;
	    rv = scm_port_ungetc (&errn, port, c);
	    SCM_ALLOW_INTS;
	    if (rv < 0)
	      scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
	    c = '#';
	    goto num;
	  }

	case 's':
	  {
	    int len;
	    SCM answer;
	    
	    SCM_DEFER_INTS;
	    c = scm_port_getc (&errn, port);
	    SCM_ALLOW_INTS;
	    if (c < 0)
	      goto read_error;
	    if (c != '"')
	      scm_throw (s_bogus_special_symbol, scm_cons (proc_read, read_params));
	    len = scm_read_string (&errn, tok_buf, port);
	    if (len < 0)
	      goto read_error;
	    
	    answer = SCM_CAR (scm_intern (SCM_STRING_CHARS (*tok_buf), len));
	    if (this_is != SCM_BOOL_F)
	      scm_hashq_set_x (cr->table, this_is, answer);
	    return answer;
	  }

	case '\\':
	  if (this_is != SCM_BOOL_F)
	    scm_wta (SCM_UNDEFINED, s_cyclic_immediate, s_read);
	  SCM_DEFER_INTS;
	  c = scm_port_getc (&errn, port);
	  SCM_ALLOW_INTS;
	  if (c == -1)
	    goto read_error;
	  j = scm_read_token (c, tok_buf, port, case_i, read_params);
	  if (j == 1)
	    return scm_int_to_char (c);
	  if (c >= '0' && c < '8')
	    {
	      p = scm_istr2int (SCM_RO_CHARS (*tok_buf), (long) j, 8);
	      if (SCM_BOOL_F != p)
		return scm_int_to_char (SCM_INUM (p));
	    }
	  for (c = 0; c < scm_n_char_names; c++)
	    if (!str_casecmp_n (scm_char_names[c].name,
				str_length (scm_char_names[c].name),
				SCM_RO_CHARS (*tok_buf), j))
	      return scm_int_to_char (scm_char_names[c].char_value);
	  scm_wta (scm_string (scm_cons (scm_makfromstr0 ("#\\"), scm_cons (*tok_buf, SCM_EOL))),
		   scm_unknown_pound,
		   s_read);


	default:
	  if (this_is != SCM_BOOL_F)
	    scm_wta (SCM_UNDEFINED, s_cyclic_immediate, s_read);
	  if (!SCM_IS_IMMEDIATE (sharp))
	    {
	      SCM got;
	      got = scm_apply3 (sharp,
			       port,
			       scm_cons (scm_listify (scm_int_to_bool (case_i),
						      scm_int_to_char (c),
						      SCM_UNDEFINED),
					 SCM_EOL), 0);
	      if (SCM_UNSPECIFIED == got)
		goto unkshrp;
	      return got;
	    }
	unkshrp:scm_wta (scm_string (scm_listify (scm_makfromstr0 ("#"),
						  scm_int_to_char (c),
						  SCM_UNDEFINED)),
			 scm_unknown_pound,
			 s_read);
	}

    case '"':
      {
	int len;
	SCM answer;
	len = scm_read_string (&errn, tok_buf, port);
	if (len < 0)
	  goto read_error;
	if (len == 0)
	  answer = scm_nullstr;
	else
	  answer = scm_makfromstr (SCM_STRING_CHARS (*tok_buf), len);
	if (this_is != SCM_BOOL_F)
	  scm_hashq_set_x (cr->table, this_is, answer);
	return answer;
      }

    case'0':case '1':case '2':case '3':case '4':
    case '5':case '6':case '7':case '8':case '9':
    case '.':
    case '-':
    case '+':
    num:
      j = scm_read_token (c, tok_buf, port, case_i, read_params);
      p = scm_istring2number (SCM_STRING_CHARS (*tok_buf), (long) j, 10L);
      if (SCM_BOOL_F != p)
	{
	  if (this_is != SCM_BOOL_F)
	    {
	      if (SCM_IS_IMMEDIATE (p))
		scm_wta (SCM_UNDEFINED, s_cyclic_immediate, s_read);
	      scm_hashq_set_x (cr->table, this_is, p);
	    }
	  return p;
	}
      if (c == '#')
	{
	  scm_wta (scm_string (scm_listify (scm_makfromstr0 ("#"),
					    *tok_buf,
					    SCM_UNDEFINED)),
		   scm_unknown_pound,
		   s_read);
	}
      goto tok;

    case ':':
      SCM_DEFER_INTS;
      c = scm_port_getc (&errn, port);
      SCM_ALLOW_INTS;
      switch (c)
	{
	case -1:
	  goto read_error;
	case SCM_WHITE_SPACES:
	case ';':
	  scm_wta (SCM_UNDEFINED, scm_empty_keyword, s_read);

	default:
	  j = scm_read_token (c, tok_buf, port, case_i, read_params);
	  break;
	}
      p = scm_intern (SCM_STRING_CHARS (*tok_buf), j);
      {
	SCM answer;
	answer = scm_symbol_to_keyword (SCM_CAR (p));
	if (this_is != SCM_BOOL_F)
	  scm_hashq_set_x (cr->table, this_is, answer);
	return answer;
      }

    default:
      j = scm_read_token (c, tok_buf, port, case_i, read_params);
      /* fallthrough */

    tok:
      if ((j == 3)
	  && (SCM_STRING_CHARS (*tok_buf)[0] == 'n')
	  && (SCM_STRING_CHARS (*tok_buf)[1] == 'i')
	  && (SCM_STRING_CHARS (*tok_buf)[2] == 'l'))
	{
	  if (this_is != SCM_BOOL_F)
	    scm_wta (SCM_UNDEFINED, s_cyclic_immediate, s_read);
	  return SCM_BOOL_F;
	}
      
      p = scm_intern (SCM_STRING_CHARS (*tok_buf), j);
      if (this_is != SCM_BOOL_F)
	scm_hashq_set_x (cr->table, this_is, SCM_CAR (p));
      return SCM_CAR (p);
    }
}


/* scm_lreadparen
 * 
 * Read a list, presuming that the first '(' has already been read.
 */
static SCM 
scm_lreadparen (SCM * tok_buf, SCM port, SCM name, int case_i, SCM sharp, SCM this_is, SCM read_params)
{
  SCM_INTS_ENABLED;
  SCM tmp;
  SCM tl;
  SCM ans;
  int c;
  int rv;
  int errn;

  c = scm_skip_ws (port, !!name, read_params);
  if (')' == c)
    {
      if (this_is != SCM_BOOL_F)
	scm_throw (s_bad_cyclic, scm_cons (proc_read, read_params));
      return SCM_EOL;
    }
  SCM_DEFER_INTS;
  rv = scm_port_ungetc (&errn, port, c);
  SCM_ALLOW_INTS;
  if (rv < 0)
    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
  tmp = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
  if (scm_i_dot == tmp)
    {
      ans = scm_lreadr (tok_buf, port, case_i, sharp, this_is, read_params);
    closeit:
      if (')' != (c = scm_skip_ws (port, !!name, read_params)))
	scm_wta (SCM_UNDEFINED, scm_missing_rparen, name);
      return ans;
    }
  ans = tl = scm_cons (tmp, SCM_EOL);
  if (this_is != SCM_BOOL_F)
    {
      struct read_cycle_record * cr;
      SCM table;

      SCM_DEFER_INTS;
      cr = read_cycle_record_ar_ref (SCM_FD (port));
      table = cr->table;
      SCM_ALLOW_INTS;
      scm_hashq_set_x (table, this_is, ans);
    }
  while (')' != (c = scm_skip_ws (port, !!name, read_params)))
    {
      SCM_DEFER_INTS;
      rv = scm_port_ungetc (&errn, port, c);
      SCM_ALLOW_INTS;
      if (rv < 0)
	scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));
      if (scm_i_dot == (tmp = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params)))
	{
	  SCM_CDR (tl) = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
	  goto closeit;
	}
      tl = (SCM_CDR (tl) = scm_cons (tmp, SCM_EOL));
    }
  return ans;
}



int
scm_read_string_list_elt (SCM * elt,
			  SCM * tok_buf,
			  int indent_width,
			  SCM port,
			  int case_i,
			  SCM sharp,
			  SCM read_params,
			  int carrot_terminates)
{
  int str_len;
  int c;
  
  /* Read characters, presuming the elt is going to be a string.
   */
  str_len = 0;
  while (1)
    {
      c = read_fn_getc (port, read_params);

    retry:
      switch (c)
	{
	default:
	  add_char_to_string (&str_len, tok_buf, c);
	  break;

	case ' ':
	case '\t':
	  add_char_to_string (&str_len, tok_buf, c);
	  break;
	  
	case '\n':
	  add_char_to_string (&str_len, tok_buf, c);
	  read_fn_read_to_column (indent_width, port, read_params);
	  break;

	case '#':
	  {
	    int c2;

	    c2 = read_fn_getc (port, read_params);
	    switch (c2)
	      {
	      default:
		add_char_to_string (&str_len, tok_buf, c);
		c = c2;
		goto retry;


	      case '#':
		c2 = read_fn_getc (port, read_params);
		add_char_to_string (&str_len, tok_buf, c2);
		break;

	      case ';':
		while (c2 != '\n')
		  c2 = read_fn_getc (port, read_params);
		read_fn_read_to_column (indent_width, port, read_params);
		break;

	      case '%':
		{
		  int depth;
		  int c2;
		  depth = 1;
		  while (depth)
		    {
		      c2 = read_fn_getc (port, read_params);
		    retry_comment:
		      switch (c2)
			{
			default:
			  break;
			case '%':
			  {
			    c2 = read_fn_getc (port, read_params);
			    if (c2 == '#')
			      --depth;
			    else
			      goto retry_comment;
			    break;
			  }
			case '#':
			  {
			    c2 = read_fn_getc (port, read_params);
			    if (c2 == '%')
			      ++depth;
			    else
			      goto retry_comment;
			    break;
			  }
			}
		    }
		}
		break;
		  
	      case ':':
	      case '/':
	      case ',':
	      case '|':
	      case '^':
		if (str_len != 0)
		  {
		    t_uchar buf[2];
		    buf[0] = c;
		    buf[1] = c2;
		    read_fn_ungetstr_n (buf, 2, port, read_params);
		    *elt = scm_makfromstr (SCM_STRING_CHARS (*tok_buf), str_len);
		    return 0;
		  }
		else if (c2 == '^')
		  {
		    SCM first;
		    if (carrot_terminates)
		      {
			read_fn_ungetstr_n ("#^", 2, port, read_params);
			goto end_of_string;
		      }
		    first = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
		    {
		      t_uchar x;
		      x = read_fn_getc (port, read_params);
		      if (x != ';')
			read_fn_ungetstr_n (&x, 1, port, read_params);
		    }
		    *elt = scm_cons (first,
				     scm_read_string_list (tok_buf,
							   indent_width,
							   port,
							   case_i,
							   sharp,
							   SCM_BOOL_F,
							   read_params,
							   1));
		    return 0;
		  }
		else
		  {
		    t_uchar buf[2];

		    if (c2 != ',')
		      {
			buf[0] = '#';
			buf[1] = c2;
			read_fn_ungetstr_n (buf, 2, port, read_params);
		      }
		    *elt = scm_lreadr (tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
		    if (c2 == ',')
		      {
			t_uchar x;
			x = read_fn_getc (port, read_params);
			if (x != ';')
			  read_fn_ungetstr_n (&x, 1, port, read_params);
		      }
		    return 0;
		  }
	      }
	    break;
	  }
	  
	case '|':
	case ':':
	case '/':
	  {
	    int c2;
	    c2 = read_fn_getc (port, read_params);
	    if (c2 != '#')
	      {
		add_char_to_string (&str_len, tok_buf, c);
		c = c2;
		goto retry;
	      }
	    c2 = read_fn_getc (port, read_params);
	    if (c2 == '#')
	      {
		add_char_to_string (&str_len, tok_buf, c);
		add_char_to_string (&str_len, tok_buf, '#');
	      }
	    else 
	      {
		t_uchar str[3];
		str[0] = c;
		str[1] = '#';
		str[2] = c2;
		read_fn_ungetstr_n (str, 3, port, read_params);

	      end_of_string:
		if (str_len != 0)
		  {
		    *elt = scm_makfromstr (SCM_STRING_CHARS (*tok_buf), str_len);
		    return 0;
		  }
		else
		  return 1;
	      }
	    break;
	  }
	}
    }

}


/* scm_read_string_list
 * 
 * Read a string list, presuming that the first '#:' has already been read.
 */
static SCM 
scm_read_string_list (SCM * tok_buf,
		      int indent_width,
		      SCM port,
		      int case_i,
		      SCM sharp,
		      SCM this_is,
		      SCM read_params,
		      int carrot_terminates)
{
  SCM_INTS_ENABLED;
  SCM answer;
  SCM * pos;
  int once;

  answer = SCM_EOL;
  pos = &answer;

  once = 0;
  while (1)
    {
      int eos;
      SCM elt;
      
      *pos = scm_cons (SCM_BOOL_F, SCM_EOL);
      if (!once)
	{
	  once = 1;
	  if (this_is != SCM_BOOL_F)
	    {
	      struct read_cycle_record * cr;
	      SCM table;
	
	      SCM_DEFER_INTS;
	      cr = read_cycle_record_ar_ref (SCM_FD (port));
	      table = cr->table;
	      SCM_ALLOW_INTS;
	      scm_hashq_set_x (table, this_is, answer);
	    }
	}

      eos = scm_read_string_list_elt (&elt,
				      tok_buf,
				      indent_width,
				      port,
				      case_i,
				      sharp,
				      read_params,
				      carrot_terminates);


      if (eos)
	break;

      SCM_CAR (*pos) = elt;
      pos = &SCM_CDR (*pos);
    }

  *pos = SCM_EOL;
  return answer;
}



/*(c read)
 * (read :optional port case? sharp :rest kws)
 * 
 * Read an object from `port' or `(current-input-port)'.
 * If `port' is a string or read-only string, read from that string.
 *
 * If `case?' is specified and not #f, symbols are read in a case
 * insensative manner (symbol names are converted to lower case).
 *
 * `sharp_reader_proc' is #f or a procedure to use to read values that
 * begin with "#" if the character following the "#" has no built-in
 * meaning. Examples of "#" syntax that is built-in are:
 *
 *	#(a b c)	;; a vector
 *	#t		;; true
 *	#f		;; false
 *	#b11010		;; a binary number
 *	#o777		;; an octal number
 *	#d1313		;; a decimal number
 *	#xceded		;; a hexadecimal number
 *	#i3.1		;; an inexact number
 *	#e69		;; an exact number
 *	#s"odd symbol"	;; a symbol with an odd name
 *	#\space		;; a character
 *
 * If `sharp' is needed, it is called:
 *
 *	(sharp port case-insensitive? first-character)
 *
 * `case-insensitive?' and `port' are as passed to `read';
 * `first-character' is the character that followed the `#'.
 *
 * Supported keyword arguments are:
 * 
 * 	:cycles		-- permit circular structures
 * 			   to be read.
 * 
 * \WARNING:/ Support for `:cycles' seems to be broken
 * in this release.
 * 
 * 
 */
SCM_PROC (s_read, "read", 0, 3, 1, scm_read);
SCM 
scm_read (SCM port, SCM case_insensative_p, SCM sharp, SCM kws)
{
  SCM_INTS_ENABLED;
  SCM_CATCH_LOCALS;
  SCM read_params;
  int c;
  SCM tok_buf;
  int case_i;
  int cycles;
  struct read_cycle_record * cr;
  struct read_cycle_record new_cr;
  struct read_cycle_record old_cr;
  int rv;
  int errn;

  if ((port == SCM_BOOL_F) || SCM_UNBNDP (port))
    port = scm_cur_inp;
  else
    SCM_ASSERT (scm_is_port (port), port, scm_arg1, s_read);

  case_i = !SCM_UNBNDP (case_insensative_p) && (SCM_BOOL_F != case_insensative_p);
  case_insensative_p = (case_i ? SCM_BOOL_T : SCM_BOOL_F);
  
  if (SCM_UNBNDP (sharp))
    sharp = SCM_BOOL_F;

  cycles = (scm_memq (kws, kw_cycles) != SCM_BOOL_F);

  read_params = scm_cons (port, scm_cons (case_insensative_p, scm_cons (sharp, kws)));

  if (SCM_BOOL_F == scm_vfdbuf_is_buffered_p (port))
    scm_throw (s_port_not_buffered, scm_cons (proc_read, read_params));

  c = scm_skip_ws (port, 0, read_params);
  if (-1 == c)
    return SCM_EOF_VAL;

  SCM_DEFER_INTS;
  rv = scm_port_ungetc (&errn, port, c);
  SCM_ALLOW_INTS;
  if (rv < 0)
    scm_throw (scm_makerrno (errn), scm_cons (proc_read, read_params));

  tok_buf = scm_makstr (30L);

  {
    SCM_DEFER_INTS;
    cr = read_cycle_record_ar_ref (SCM_FD (port));
    if (!cycles)
      {
	new_cr.table = 0;
      }
    else if (!cr->table)
      new_cr.table = scm_makvector (63, SCM_EOL, 0, SCM_BOOL_F);
    else
      new_cr.table = cr->table;
    old_cr = *cr;
    SCM_ALLOW_INTS;
    if (new_cr.table)
      scm_coextensive (port, new_cr.table);
  }
  
  SCM_CATCH_INIT(SCM_BOOL_T);
  if (SCM_CATCH_SETJMP())
    {
      SCM_DEFER_INTS;
      *cr = old_cr;
      SCM_ALLOW_INTS;
      SCM_UNWIND;
      return scm_throw (SCM_THROW_TAG, SCM_THROW_ARGS);
    }
  else
    {
      SCM answer;
      SCM_CATCH_BODY_BEGIN;
      SCM_DEFER_INTS;
      *cr = new_cr;
      SCM_ALLOW_INTS;
      answer = scm_lreadr (&tok_buf, port, case_i, sharp, SCM_BOOL_F, read_params);
      SCM_DEFER_INTS;
      *cr = old_cr;
      SCM_ALLOW_INTS;
      SCM_CATCH_BODY_END;
      return answer;
    }
}


/****************************************************************
 * {Names of immediate symbols}
 * 
 * This table must agree with the declarations in scm.h: {Immediate Symbols}.
 * Horrible, ain't it?
 */

char *scm_isymnames[] =
{
#undef SCM_SPECIAL_IMMEDIATE
#define SCM_SPECIAL_IMMEDIATE(NAME,VALUE,PRINTNAME)	PRINTNAME
  SCM_SPECIAL_IMMEDIATES
};



/* scm_put_char
 * 
 * Write `c' on `port'.  If `writing', print the external 
 * representation (e.g. "#\a", not "a").
 */
static void
scm_put_char (int c, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  int errn;

  if (writing)
    scm_port_puts (&errn, port, "#\\");
  if (!writing)
    scm_port_putc (&errn, port, (t_uchar)c);
  else if ((c <= ' ') && scm_char_names[c].name)
    scm_port_puts (&errn, port, scm_char_names[c].name);
  else if (c > '\177')
    scm_intprint (c, 8, port);
  else
    scm_port_putc (&errn, port, (int) c);
}


/* scm_iprlist
 * 
 * Print the elements of the list `exp', separated by spaces.
 * Use dot notation of `exp' is not a proper list.
 * Write `tlr' at the end of the list.
 *
 * If `writing', use `write' for the elements, otherwise use
 * `display'.
 */
static void 
scm_iprlist (char *hdr, SCM exp, char tlr, SCM port, int writing)
{
  SCM_INTS_ENABLED;
  int errn;

  SCM_DEFER_INTS;
  scm_port_puts (&errn, port, hdr);
  SCM_ALLOW_INTS;
  scm_iprin1 (SCM_CAR (exp), port, writing);
  exp = SCM_CDR (exp);
  for (; !SCM_IS_IMMEDIATE (exp); exp = SCM_CDR (exp))
    {
      if (SCM_NECONSP (exp))
	break;

      {
	struct cycle_record * cr;

	SCM_DEFER_INTS;
	cr = cycle_record_ar_ref (SCM_FD (port));
	SCM_ALLOW_INTS;

	if (   cr->table
	    && !SCM_IS_IMMEDIATE (exp)
	    && (   cr->strict
		|| (   SCM_CONSP (exp)
		    || scm_is_vector (exp))))
	  {
	    SCM prev;
	    SCM twice;

	    prev = scm_hashq_ref (cr->table, exp, SCM_BOOL_F);
	    twice = scm_hashq_ref (cr->twice, exp, SCM_BOOL_F);
	    if (cr->pass == 1)
	      {
		if (prev == SCM_BOOL_F)
		  {
		    long n;
		    n = cr->obj_number++;
		    scm_hashq_set_x (cr->table, exp, scm_long2num (n));
		  }
		else
		  {
		    if (twice == SCM_BOOL_F)
		      scm_hashq_set_x (cr->twice, exp, prev);
		  dot_cycle:
		    {
		      SCM_DEFER_INTS;
		      scm_port_write (&errn, port, " . ", 3);
		      scm_iprin1 (exp, port, writing);
		      scm_port_putc (&errn, port, tlr);
		      SCM_ALLOW_INTS;
		      return;
		    }		    
		  }
	      }
	    else /* pass == 2 */
	      {
		if (twice != SCM_BOOL_F)
		  goto dot_cycle;
	      }
	  }
      }
      SCM_DEFER_INTS;
      scm_port_putc (&errn, port, ' ');
      SCM_ALLOW_INTS;
      scm_iprin1 (SCM_CAR (exp), port, writing);
    }
  if (SCM_EOL != exp)
    {
      SCM_DEFER_INTS;
      scm_port_puts (&errn, port, " . ");
      SCM_ALLOW_INTS;
      scm_iprin1 (exp, port, writing);
    }
  SCM_DEFER_INTS;
  scm_port_putc (&errn, port, tlr);
  SCM_ALLOW_INTS;
}


static void
scm_print_string (SCM string, SCM port)
{
  t_uchar * chrs;
  int len;
  int errn;
  int i;
  
  SCM_DEFER_INTS;
  scm_port_putc (&errn, port, '\"');
  SCM_ALLOW_INTS;

  chrs = SCM_RO_CHARS (string);
  len = SCM_RO_LENGTH (string);

  for (i = 0; i < len; ++i)
    {
      SCM_DEFER_INTS;
      scm_port_puts (&errn, port, char_name[chrs[i]]);
      SCM_ALLOW_INTS;
    }

  SCM_DEFER_INTS;
  scm_port_putc (&errn, port, '\"');
  SCM_ALLOW_INTS;
  scm_return_first (string, port);
}

#define RXOR(A,B)			"[[:([[:(" A "):]]\\|[[:(" B "):]]):]]"
#define RXPLUS(A)			"[[:([[:(" A "):]]\\+):]]"
#define RXSTAR(A)			"[[:([[:(" A "):]]*):]]"
#define RXPAREN(A)			"[[:(" A "):]]"

/* Identifier Syntax
 *
 * This syntax is extended beyond R4RS to include
 *
 *	[0-9]\+[*-+/]
 *
 * Which permits common-lisp style function names `1+' and `1-'.
 */
#define identifier_regexp		RXOR(RXOR (initial_regexp RXSTAR (subsequent_regexp), peculiar_identifier_regexp), extended_identifier_regexp)
#define extended_identifier_regexp	RXPLUS (digits_regexp) "[-+*/]"
#define initial_regexp			RXOR (letter_regexp, special_initial_regexp)
#define letter_regexp			"[a-zA-Z]"
#define special_initial_regexp		"[!$%&*/:<=>?~_^]"
#define subsequent_regexp		RXOR (initial_regexp, RXOR (digits_regexp, special_subsequent_regexp))
#define digits_regexp			"[0-9]"
#define special_subsequent_regexp	"[-.+]"
#define peculiar_identifier_regexp	RXOR ("+", RXOR ("-", "\\.\\.\\."))
#define normal_symbol_regexp 		"^" RXPAREN(identifier_regexp) "$"


/* Number Syntax
 */
#define num_regexp			"^[[:("      "[[:(" prefix_regexp(2) complex_regexp(2) "):]]" \
					       "\\|" "[[:(" prefix_regexp(8) complex_regexp(8) "):]]" \
					       "\\|" "[[:(" prefix_regexp(10) complex_regexp(10) "):]]" \
					       "\\|" "[[:(" prefix_regexp(16) complex_regexp(16) "):]]):]]$"
#define complex_regexp(N)		"[[:("      "[[:(" real_regexp(N) "):]]" \
					      "\\|" "[[:(" real_regexp(N) "@" real_regexp(N) "):]]" \
					      "\\|" "[[:(" real_regexp(N) "+" real_regexp(N) "i" "):]]" \
					      "\\|" "[[:(" real_regexp(N) "-" real_regexp(N) "i" "):]]" \
					      "\\|" "[[:(" real_regexp(N) "+i" "):]]" \
					      "\\|" "[[:(" real_regexp(N) "-i" "):]]" \
					      "\\|" "[[:(" "+" ureal_regexp(N) "i" "):]]" \
					      "\\|" "[[:(" "-" ureal_regexp(N) "i" "):]]" \
					      "\\|" "[[:(" "+i" "):]]" \
					      "\\|" "[[:(" "-i" "):]]):]]"
#define real_regexp(N)			sign_regexp ureal_regexp(N)
#define ureal_regexp(N)			ureal ## N ## _regexp
#define ureal_generic_regexp(N)		RXOR(uinteger_regexp(N), uinteger_regexp(N) "/" uinteger_regexp(N))
#define ureal2_regexp			ureal_generic_regexp(2)
#define ureal8_regexp			ureal_generic_regexp(8)
#define ureal16_regexp			ureal_generic_regexp(16)
#define ureal10_regexp			RXOR(ureal_generic_regexp(10), decimal10_regexp)
#define decimal10_regexp		"[[:("      "[[:(" uinteger_regexp(10) suffix_regexp "):]]" \
					      "\\|" "[[:(" "\\." RXPLUS(digit_regexp(10)) "#*" suffix_regexp "):]]" \
					      "\\|" "[[:(" RXPLUS(digit_regexp(10)) "\\." RXPLUS(digit_regexp(10)) "#*" suffix_regexp "):]]" \
					      "\\|" "[[:(" RXPLUS(digit_regexp(10)) "#\\+\\.#*" suffix_regexp "):]]):]]"
#define uinteger_regexp(RADIX)		RXPLUS(digit_regexp(RADIX)) "#*"
#define prefix_regexp(RADIX)		RXOR(radix_regexp(RADIX) exactness_regexp, exactness_regexp radix_regexp(RADIX))
#define suffix_regexp			RXOR("", exponent_marker_regexp sign_regexp RXPLUS(digit10_regexp))
#define exponent_marker_regexp		"[esfdl]"
#define sign_regexp			"[[:(\\|[-+]):]]"
#define exactness_regexp		"[[:(\\|#[ie]):]]"
#define radix2_regexp 			"#b"
#define radix8_regexp			"#o"
#define radix10_regexp			"[[:(\\|#d):]]"
#define radix16_regexp			"#x"
#define radix_regexp(N)			radix ## N ## _regexp
#define digit2_regexp			"[01]"
#define digit8_regexp			"[0-7]"
#define digit10_regexp			"[0-9]"
#define digit16_regexp			"[0-9a-fA-F]"
#define digit_regexp(N)			digit ## N ## _regexp




static regex_t *
normal_symbol_regex_t ()
{
  static regex_t * answer = 0;
  static regex_t storage;

  if (!answer)
    {
      if (regcomp (&storage, normal_symbol_regexp, 0))
	panic ("read-print.c: unable to compile normal_symbol_regexp");
      answer = &storage;
    }
  return answer;
}


static regex_t *
number_regex_t ()
{
  static regex_t * answer = 0;
  static regex_t storage;

  if (!answer)
    {
      if (regcomp (&storage, num_regexp, 0))
	panic ("read-print.c: unable to compile num_regexp");
      answer = &storage;
    }
  return answer;
}


/* scm_iprin1
 * 
 * Print generally.  Handles both write and display according to `writing'.
 */
void 
scm_iprin1 (SCM exp, SCM port, int writing)
{
  SCM_INTS_ENABLED;
  struct cycle_record * cr;
  long i;
  int errn;

  SCM_DEFER_INTS;
  cr = cycle_record_ar_ref (SCM_FD (port));
  SCM_ALLOW_INTS;

 taloop:
  if (   cr->table
      && !SCM_IS_IMMEDIATE (exp)
      && (   cr->strict
	  || (   SCM_CONSP (exp)
	      || scm_is_vector (exp))))
    {
      SCM prev;
      SCM twice;

      prev = scm_hashq_ref (cr->table, exp, SCM_BOOL_F);
      twice = scm_hashq_ref (cr->twice, exp, SCM_BOOL_F);
      if (cr->pass == 1)
	{
	  if (prev == SCM_BOOL_F)
	    {
	      long n;
	      n = cr->obj_number++;
	      scm_hashq_set_x (cr->table, exp, scm_long2num (n));
	    }
	  else
	    {
	      if (twice == SCM_BOOL_F)
		scm_hashq_set_x (cr->twice, exp, prev);
	    print_cycle:
	      {
		long n;
		SCM_DEFER_INTS;
		scm_port_write (&errn, port, "##", 2);
		SCM_ALLOW_INTS;
		n = scm_num2long (prev,
				  s_cyclic_reference_number,
				  (writing ? s_write : s_display));
		SCM_DEFER_INTS;
		scm_intprint (n, 10, port);
		SCM_ALLOW_INTS;
		return;
	      }
	    }
	}
      else /* cr->pass == 2 */
	{
	  if (   (prev == SCM_BOOL_F)
	      && (twice != SCM_BOOL_F))
	    {
	      long n;
	      scm_hashq_set_x (cr->table, exp, twice);
	      SCM_DEFER_INTS;
	      scm_port_write (&errn, port, "##", 2);
	      SCM_ALLOW_INTS;
	      n = scm_num2long (twice,
				s_cyclic_reference_number,
				(writing ? s_write : s_display));
	      SCM_DEFER_INTS;
	      scm_intprint (n, 10, port);
	      scm_port_write (&errn, port, "=", 1);
	      SCM_ALLOW_INTS;
	    }
	  else if (prev != SCM_BOOL_F)
	    goto print_cycle;
	}
    }
  switch (7 & (int) exp)
    {
    case 2:
    case 6:
      scm_intprint (SCM_INUM (exp), 10, port);
      break;
    case 4:
      if (scm_is_char (exp))
	{
	  i = scm_char_to_int (exp);
	  scm_put_char (i, port, writing);

	}
      else if (   SCM_IFLAGP (exp)
	       && (SCM_ISYMNUM (exp) < (sizeof scm_isymnames / sizeof (char *))))
	{
	  SCM_DEFER_INTS;
	  scm_port_puts (&errn, port, SCM_ISYM_CHARS (exp));
	  SCM_ALLOW_INTS;
	}
      else if (SCM_ILOCP (exp))
	{
	  SCM_DEFER_INTS;
	  scm_port_puts (&errn, port, "#@");
	  SCM_ALLOW_INTS;
	  scm_intprint ((long) SCM_IFRAME (exp), 10, port);
	  SCM_DEFER_INTS;
	  scm_port_putc (&errn, port, SCM_ICDRP (exp) ? '-' : '+');
	  SCM_ALLOW_INTS;
	  scm_intprint ((long) SCM_IDIST (exp), 10, port);
	}
      else
	goto idef;
      break;
    case 1:
      /* gloc */
      SCM_DEFER_INTS;
      scm_port_puts (&errn, port, "#@");
      SCM_ALLOW_INTS;
      exp = SCM_CAR (exp - 1);
      goto taloop;
    default:
    idef:
      scm_ipruk ("immediate", exp, port);
      break;
    case 0:
      switch (SCM_TYP7 (exp))
	{
	case scm_tcs_cons_gloc:
	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  scm_iprlist ("(", exp, ')', port, writing);
	  break;
	case scm_tcs_closures:
	  {
	    SCM name;
	    name = scm_procedure_property (exp, s_procedure_print_name);
	    if (scm_is_ro_string (name))
	      {
		SCM_DEFER_INTS;
		scm_port_puts (&errn, port, "#<procedure");
		scm_port_putc (&errn, port, ' ');
		scm_port_write (&errn, port, SCM_RO_CHARS (name), SCM_RO_LENGTH (name));
		scm_port_putc (&errn, port, '>');
		SCM_ALLOW_INTS;
	      }
	    else
	      {
		exp = SCM_CODE (exp);
		scm_iprlist ("#<CLOSURE ", exp, '>', port, writing);
	      }
	    break;
	  }

	case scm_tc7_string:
	case scm_tc7_substring:
	case scm_tc7_subsymbol:
	case scm_tc7_static_string:
	case scm_tc7_static_substring:
	case scm_tc7_static_subsymbol:
	  if (!writing)
	    {
	      SCM_DEFER_INTS;
	      scm_port_write (&errn,
			      port,
			      SCM_RO_CHARS (exp),
			      (size_t) SCM_RO_LENGTH (exp));
	      SCM_ALLOW_INTS;
	    }
	  else
	    scm_print_string (exp, port);
	  break;

	case scm_tcs_symbols:
	  {
	    t_uchar * chrs;
	    int len;
	    int weird;
	    
	    chrs = SCM_RO_CHARS (exp);
	    len = SCM_RO_LENGTH (exp);
	    weird = (   regnexec (normal_symbol_regex_t (), chrs, len, 0, 0, 0)
		     || !regnexec (number_regex_t (), chrs, len, 0, 0, 0));
	    if (!weird)
	      {
		SCM_DEFER_INTS;
		scm_port_write (&errn, port, chrs, len);
		SCM_ALLOW_INTS;
	      }
	    else
	      {
		SCM_DEFER_INTS;
		scm_port_write (&errn, port, "#s", 2);
		SCM_ALLOW_INTS;
		scm_print_string (exp, port);
	      }
	    scm_return_first (exp, port);
	    break;
	  }
	    
	case scm_tc7_wvect:
	  SCM_DEFER_INTS;
	  if (scm_is_weak_key_hash_table (exp))
	    scm_port_puts (&errn, port, "#wk(");
	  else if (scm_is_weak_value_hash_table (exp))
	    scm_port_puts (&errn, port, "#wv(");
	  else if (scm_is_doubly_weak_hash_table (exp))
	    scm_port_puts (&errn, port, "#wkv(");
	  else
	    scm_port_puts (&errn, port, "#w(");
	  SCM_ALLOW_INTS;
	  goto common_vector_printer;

	case scm_tc7_vector:
	  SCM_DEFER_INTS;
	  scm_port_puts (&errn, port, "#(");
	  SCM_ALLOW_INTS;
	common_vector_printer:
	  for (i = 0; i + 1 < SCM_LENGTH (exp); ++i)
	    {
	      scm_iprin1 (SCM_VECTOR_ELTS (exp)[i], port, writing);
	      SCM_DEFER_INTS;
	      scm_port_putc (&errn, port, ' ');
	      SCM_ALLOW_INTS;
	    }
	  if (i < SCM_LENGTH (exp))
	    scm_iprin1 (SCM_VECTOR_ELTS (exp)[i], port, writing);
	  SCM_DEFER_INTS;
	  scm_port_putc (&errn, port, ')');
	  SCM_ALLOW_INTS;
	  break;

	case scm_tcs_subrs:
	  {
	    SCM name;
	    name = scm_procedure_property (exp, s_procedure_print_name);
	    SCM_DEFER_INTS;
	    if (scm_is_ro_string (name))
	      {
		scm_port_puts (&errn, port, "#<procedure");
		scm_port_putc (&errn, port, ' ');
		scm_port_write (&errn, port, SCM_RO_CHARS (name), SCM_RO_LENGTH (name));
		scm_port_putc (&errn, port, '>');
	      }
	    else
	      {
		scm_port_puts (&errn, port, "#<primitive-procedure ");
		name = SCM_SNAME (exp);
		scm_port_write (&errn, port, SCM_RO_CHARS (name), SCM_RO_LENGTH (name));
		scm_port_putc (&errn, port, '>');
	      }
	    SCM_ALLOW_INTS;
	    break;
	  }
	case scm_tc7_cclo:
	  {
	    SCM name;
	    name = scm_procedure_property (exp, s_procedure_print_name);
	    if (scm_is_ro_string (name))
	      {
		SCM_DEFER_INTS;
		scm_port_puts (&errn, port, "#<compiled-closure");
		scm_port_putc (&errn, port, ' ');
		scm_port_write (&errn, port, SCM_RO_CHARS (name), SCM_RO_LENGTH (name));
		scm_port_putc (&errn, port, '>');
		SCM_ALLOW_INTS;
	      }
	    else
	      {
		SCM_DEFER_INTS;
		scm_port_puts (&errn, port, "#<compiled-closure ");
		SCM_ALLOW_INTS;
		scm_iprin1 (SCM_CCLO_SUBR (exp), port, writing);
		SCM_DEFER_INTS;
		scm_port_putc (&errn, port, '>');
		SCM_ALLOW_INTS;
	      }
	    break;
	  }
	  break;
	case scm_tc7_contin:
	  SCM_DEFER_INTS;
	  scm_port_puts (&errn, port, "#<continuation ");
	  SCM_ALLOW_INTS;
	  scm_intprint (SCM_LENGTH (exp), 10, port);
	  SCM_DEFER_INTS;
	  scm_port_puts (&errn, port, " @ ");
	  SCM_ALLOW_INTS;
	  scm_intprint ((long) SCM_CDR (exp), 16, port);
	  SCM_DEFER_INTS;
	  scm_port_putc (&errn, port, '>');
	  SCM_ALLOW_INTS;
	  break;
	case scm_tc7_smob:
	  i = SCM_SMOBNUM (exp);
	  if (i < scm_numsmob && scm_smobs[i].print)
	    {
	      int ok;
	      SCM_DEFER_INTS;
	      ok = (scm_smobs[i].print) (exp, port, writing);
	      SCM_ALLOW_INTS;
	      if (ok)
		break;
	    }
	  goto punk;

	default:
	punk:scm_ipruk ("type", exp, port);
	}
    }
}


/* scm_intprint
 * 
 * Print an integer in the given base (but without any "#?" prefix).
 */
void 
scm_intprint (long n, int radix, SCM port)
{
  SCM_INTS_DISABLED;
  char num_buf[SCM_INTBUFLEN];
  int errn;

  scm_port_write (&errn, port, num_buf, scm_iint2str (n, radix, num_buf));
}


/* scm_ipruk
 * 
 * Print an object of unrecognized type.
 */
void 
scm_ipruk (char *hdr, SCM ptr, SCM port)
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, "#<unknown-");
  scm_port_puts (&errn, port, hdr);
  if (SCM_CELLP (ptr))
    {
      scm_port_puts (&errn, port, " (0x");
      scm_intprint (SCM_CAR (ptr), 16, port);
      scm_port_puts (&errn, port, " . 0x");
      scm_intprint (SCM_CDR (ptr), 16, port);
      scm_port_puts (&errn, port, ") @");
    }
  scm_port_puts (&errn, port, " 0x");
  scm_intprint (ptr, 16, port);
  scm_port_putc (&errn, port, '>');
}



static SCM
scm_write_or_display (SCM obj, SCM port, SCM kws, int writing)
{
  SCM_INTS_ENABLED;
  SCM_CATCH_LOCALS;
  int strict;
  int cycles;
  int cycles1;
  int like;
  SCM pass1_port;
  struct cycle_record * cr;

  if (SCM_UNBNDP (port) || (port == SCM_BOOL_F))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_is_port (port),
		port,
		scm_arg2,
		(writing ? s_write : s_display));

  if (SCM_UNBNDP (kws))
    kws = SCM_BOOL_F;

  strict = scm_memq (kw_strict, kws) != SCM_BOOL_F;
  cycles = scm_memq (kw_cycles, kws) != SCM_BOOL_F;
  cycles1 = scm_memq (kw_cycles1, kws) != SCM_BOOL_F;
  like = scm_memq (kw_like, kws) != SCM_BOOL_F;

  SCM_ASSERT (!(like && cycles && cycles1),
	      kws,
	      s_cycles_cycles1,
	      (writing ? s_write : s_display));

  if (!(like || cycles || cycles1))
    scm_iprin1 (obj, port, writing);
  else
    {
      struct cycle_record new_cr;
      struct cycle_record old_cr;

      if (cycles1 || cycles)
	{
	  if (cycles1)
	    pass1_port = port;
	  else
	    {
	      pass1_port = scm_sys_virtual_null_fd (SCM_MAKINUM (O_WRONLY));
	      if (!scm_is_port (pass1_port))
		scm_throw (pass1_port, scm_cons (proc_write, scm_cons (obj, scm_cons (port, kws))));
	    }
	  new_cr.strict = strict;
	  new_cr.pass = 1;
	  new_cr.obj_number = 0;
	  new_cr.table = scm_makvector (63, SCM_EOL, 0, SCM_BOOL_F);
	  new_cr.twice = scm_makvector (63, SCM_EOL, 0, SCM_BOOL_F);
	  scm_coextensive (pass1_port, new_cr.table);
	  scm_coextensive (pass1_port, new_cr.twice);
	}
      else
	{
	  SCM like_port;
	  struct cycle_record * like_cr;

	  pass1_port = port;
	  like_port = scm_kw_arg_ref (kws, kw_like, SCM_BOOL_F);
	  SCM_ASSERT (scm_is_port (like_port),
		      like_port,
		      scm_bad_kw_arg,
		      (writing ? s_write : s_display));
	  SCM_DEFER_INTS;
	  like_cr = cycle_record_ar_ref (SCM_FD (like_port));
	  new_cr = *like_cr;
	  SCM_ALLOW_INTS;
	  if (new_cr.table)
	    {
	      scm_coextensive (pass1_port, new_cr.table);
	      scm_coextensive (pass1_port, new_cr.twice);
	    }
	}

      SCM_DEFER_INTS;
      cr = cycle_record_ar_ref (SCM_FD (pass1_port));
      old_cr = *cr;
      SCM_ALLOW_INTS;

    like_loop:
      SCM_CATCH_INIT (SCM_BOOL_T);
      if (SCM_CATCH_SETJMP())
	{
	  SCM_DEFER_INTS;
	  *cr = old_cr;
	  SCM_ALLOW_INTS;
	  SCM_UNWIND;
	  return scm_throw (SCM_THROW_TAG, SCM_THROW_ARGS);
	}
      else
	{
	  SCM_CATCH_BODY_BEGIN;
	  SCM_DEFER_INTS;
	  *cr = new_cr;
	  SCM_ALLOW_INTS;
	  scm_iprin1 (obj, pass1_port, writing);
	  SCM_DEFER_INTS;
	  *cr = old_cr;
	  SCM_ALLOW_INTS;
	  SCM_CATCH_BODY_END;
	}
      if (cycles)
	{
	  cycles = 0;
	  like = 1;
	  new_cr.pass = 2;
	  new_cr.table = scm_makvector (63, SCM_EOL, 0, SCM_BOOL_F);
	  scm_close_port (pass1_port);
	  pass1_port = port;
	  SCM_DEFER_INTS;
	  cr = cycle_record_ar_ref (SCM_FD (port));
	  SCM_ALLOW_INTS;
	  goto like_loop;
	}
    }
  return SCM_UNSPECIFIED;
}


/*(c write)
 * (write obj :optional port :rest kw-arguments)
 * 
 *  Write a printed representation of `obj' on `port' or
 *  `(current-output-port)'.
 *
 *  For all objects that can be read, `write' prints using that
 *  syntax.
 * 
 * \WARNING:/ Keyword arguments control the printing of circular
 * structures, but seem not to work correctly in the current release.
 * 
 */
SCM_PROC(s_write, "write", 1, 1, 1, scm_write);
SCM 
scm_write (SCM obj, SCM port, SCM kws)
{
  SCM_INTS_ENABLED;
  return scm_write_or_display (obj, port, kws, 1);
}


/*(c display)
 * (display obj :optional port)
 * 
 *  Write a printed representation of `obj' on `port' or
 *  `(current-output-port)'.
 *
 * `display' is similar to `write', except that strings are printed
 * without quotes, characters are printed without "#\" and so forth.
 * 
 * \WARNING:/ Keyword arguments control the printing of circular
 * structures, but seem not to work correctly in the current release.
 */
SCM_PROC(s_display, "display", 1, 1, 1, scm_display);
SCM 
scm_display (SCM obj, SCM port, SCM kws)
{
  SCM_INTS_ENABLED;
  return scm_write_or_display (obj, port, kws, 0);
}

/*(c display*)
 * (display* . args)
 * 
 * This is simply:
 * 	(define (display* . args) (for-each display args))
 */

/*(c display*-port)
 * (display*-port port . args)
 * 
 * This is simply:
 * 
 *   (define (display*-port port . args)
 *     (if port
 *         (with-output-to-port port (lambda () (apply display* args)))
 *         (apply display* args)))
 */


/*(c newline)
 * (newline :optional port)
 * 
 * Write the character #\nl to `port' or to `(current-output-port)'.
 */
SCM_PROC(s_newline, "newline", 0, 1, 0, scm_newline);
SCM
scm_newline(SCM port)
{
  SCM_INTS_ENABLED;
  int errn;

  if (SCM_UNBNDP (port))
    port = scm_cur_outp;
  else
    SCM_ASSERT (scm_is_port (port), port, scm_arg1, s_newline);
  SCM_DEFER_INTS;
  scm_port_putc (&errn, port, '\n');
  if (port == scm_cur_outp)
    scm_port_flush (&errn, port);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}




void
scm_init_read_print (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/read-print.x"
}
