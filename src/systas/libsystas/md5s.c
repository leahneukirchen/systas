/* md5s.c: 
 *
 ****************************************************************
 * Copyright (C) 2002 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include <stdlib.h>
#include <setjmp.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/md5.h"
#include "systas/libsystas/systas.h"
#include "systas/libsystas/md5s.h"



static long scm_tc16_md5_t;
#define MD5(X)	((md5_state_t *)SCM_CDR(X))


/* __STDC__ prototypes for static functions */
static size_t free_md5_t (SCM obj);
static int print_md5_t (SCM obj, SCM port, int writing);


/************************************************************************
 *(h0 "MD5 Checksums")
 * 
 * A straighforward md5 engine.
 * 
 * Thanks to Aladdin Enterprises for creating a GPL-compatible MD5
 * implementation.
 * 
 */
/*(menu)
 */


SCM_PROC (s_md5_state_p, "md5-state?", 1, 0, 0, scm_md5_state_p);
SCM
scm_md5_state_p (SCM obj)
{
  return (scm_is_md5_state (obj) ? SCM_BOOL_T : SCM_BOOL_F);
}



SCM_PROC (s_make_md5_engine, "make-md5-engine", 0, 0, 0, scm_make_md5_engine);
SCM
scm_make_md5_engine (void)
{
  SCM answer;

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  {
    md5_state_t * state;

    state = (md5_state_t *)scm_must_malloc (sizeof (*state));
    md5_init (state);
    SCM_CAR (answer) = scm_tc16_md5_t;
    SCM_CDR (answer) = (SCM)state;
  }
  SCM_ALLOW_INTS;
  return answer;
}

SCM_PROC (s_md5_append_x, "md5-append!", 2, 0, 0, scm_md5_append_x);
SCM
scm_md5_append_x (SCM engine, SCM str)
{
  SCM_ASSERT (scm_is_md5_state (engine), engine, scm_arg1, s_md5_append_x);
  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg2, s_md5_append_x);

  SCM_DEFER_INTS;
  {
    md5_state_t * state;
    t_uchar * bytes;
    int len;

    scm_remember (&str);
    scm_remember (&engine);
    state = MD5 (engine);
    len = SCM_RO_LENGTH (str);
    bytes = SCM_RO_CHARS (str);

    md5_append (state, bytes, len);
  }
  SCM_ALLOW_INTS;
  return engine;
}

SCM_PROC (s_md5_finish_x, "md5-finish!", 1, 0, 0, scm_md5_finish_x);
SCM
scm_md5_finish_x (SCM engine)
{
  SCM answer;
  SCM_ASSERT (scm_is_md5_state (engine), engine, scm_arg1, s_md5_finish_x);

  SCM_DEFER_INTS;
  {
    md5_state_t * state;
    t_uchar digest[16];
    t_uchar hex_version[32];
    int x;

    scm_remember (&engine);
    state = MD5 (engine);
    md5_finish (state, digest);

    for (x = 0; x < 16; ++x)
      {
#	define hex_digit(N) ((N) >= 10 ? (((N) - 10) + 'a') : ((N) + '0'))

	hex_version[x * 2] = hex_digit(digest[x] >> 4);
	hex_version[x * 2 + 1] = hex_digit(digest[x] & 0xf);
      }

    answer = scm_makfromstr (hex_version, 32);
    md5_init (state);
  }
  SCM_ALLOW_INTS;
  return answer;
}



static size_t
free_md5_t (SCM obj)
{
  md5_state_t * state;

  state = MD5(obj);
  scm_must_free ((char *)state);
  return sizeof (md5_state_t);
}

static int
print_md5_t (SCM obj, SCM port, int writing)
{
  md5_state_t * state;
  int errn;

  state = MD5 (obj);
  scm_port_puts (&errn, port, "#<md5 0x");
  scm_intprint ((long)((unsigned long)state & 0xffff), 16, port);
  scm_port_puts (&errn, port, ">");
  return 1;
}

static scm_small_object_functions md5_t_smob =
{ scm_mark0, free_md5_t, print_md5_t, 0 };

int
scm_is_md5_state (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_CAR (obj) == (SCM)scm_tc16_md5_t);
}



void
scm_init_md5s (void)
{
  scm_tc16_md5_t = scm_newsmob (&md5_t_smob);
#include "systas/libsystas/md5s.x"
}




/* tag: Tom Lord Sat Apr 20 09:33:04 2002 (md5s.c)
 */
