/* systas-raw.c:
 *
 ****************************************************************
 *	Copyright (C) 1996,1997 Free Software Foundation, Inc. 
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */


#include "config-include/config-options.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fs/file-names.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/vu/safe.h"
#include "unexec/libunexec/unexec.h"
#include "systas/libsystas/systas.h"



void
scm_appinit (void)
{
  scm_intern_symhash ("hackerlab-version", scm_makfromstr0 ("1.0"));
}

static void * scm_unexec_saved_brk = 0;

#define PROFILING 0

int 
main (int argc, char * argv[])
{
  static int been_there_done_that = 0;
  t_uchar * arg0_file;
  t_uchar * PATH;
  t_uchar ** path;
  int is_development_version;
  int boot_stat;
  int errn;
  char * boot_result;

  if (been_there_done_that)
    {
      void * res;


      res = brk (scm_unexec_saved_brk);
      if (res)
	panic ("unable to retore brk");

#ifdef __FreeBSD__
#if PROFILING
      {
	extern void _mcleanup ();       
	extern char etext;
	extern _start ();

	atexit (_mcleanup);
	monstartup (_start, &etext);
      }
#endif
#endif
      /* Gerd Moellmann <gerd@acm.org> says this makes profiling work on
	 FreeBSD.  It might work on some other systems too.
	 Give it a try and tell me if it works on your system.  */
      vu_reinit_after_unexec ();
      lim_free (0, scm_unexec_request_filename);
      scm_unexec_request_filename = 0;
    }
  else
    {
      rx_set_dfa_cache_threshold (1 * 1024 * 1024);
      rx_set_nfa_cache_threshold (2 * 1024 * 1024);
    }



  PATH = getenv ("PATH");
  if (!PATH)
    PATH = "/bin:/usr/bin";
  path = path_parse (lim_use_must_malloc, PATH);

  /* Are we running the development version or the 
   * installed version?
   */
  {
    t_uchar * arg0_tail;
    t_uchar * devo_file;
    struct stat devo_stat;
    int devo_ok;
    int devo_dev;
    int devo_ino;
    t_uchar * arg0;
    t_uchar * arg0_slash;
    struct stat arg0_stat;
    int arg0_ok;
    int arg0_dev;
    int arg0_ino;

    arg0_tail = file_name_tail (lim_use_must_malloc, argv[0]);
    devo_file = file_name_in_vicinity (lim_use_must_malloc, BUILDDIR, arg0_tail);
    if (0 > vu_stat (&errn, devo_file, &devo_stat))
      {
	devo_ok = 0;
      }
    else
      {
	devo_ok = 1;
	devo_dev = devo_stat.st_dev;
	devo_ino = devo_stat.st_ino;
      }

    arg0 = argc ? argv[0] : (char *)"systas";
    arg0_slash = str_chr_index (arg0, '/');
    if (arg0_slash)
      arg0_file = arg0;
    else
      arg0_file = path_find_executable (lim_use_must_malloc, path, arg0);

    if (!arg0_file)
      {
	arg0_ok =  0;
      }
    else

      {
	if (0 > vu_stat (&errn, arg0_file, &arg0_stat))
	  {
	    arg0_ok = 0;
	  }
	else
	  {
	    arg0_ok = 1;
	    arg0_dev = arg0_stat.st_dev;
	    arg0_ino = arg0_stat.st_ino;
	  }
      }

    is_development_version = (devo_ok && arg0_ok && (arg0_dev == devo_dev) && (arg0_ino == devo_ino));

    if (been_there_done_that)
      {
	SCM devo_var;

	devo_var = scm_intern0 ("development-version?");
	SCM_CDR (devo_var) = (is_development_version ? SCM_BOOL_T : SCM_BOOL_F);
      }
  }

  /* Boot the Scheme interpreter.
   */
  if (been_there_done_that)
    boot_stat = scm_boot_systas (&boot_result, argc, argv, 0, 1, 2, "(begin (start))");
  else
    boot_stat = scm_boot_systas (&boot_result, argc, argv, 0, 1, 2, (is_development_version ? DEVO_BOOT_CMD : BOOT_CMD));

  /* Print a diagnostic message or the final result and exit.
   */
  switch (boot_stat)
    {
    case scm_boot_ok:
      if (scm_unexec_request_filename)
	{
#if cfg__systas_unexec
	  SCM_STACKITEM i;
	  been_there_done_that = 1;
	  scm_restart_stack (&i);
	  scm_igc ();
	  scm_igc ();
	  safe_unbuffer (0);
	  safe_unbuffer (1);
	  safe_unbuffer (2);
	  scm_unexec_saved_brk = sbrk (0);
	  if (unexec (&errn, scm_unexec_request_filename, arg0_file))
	    panic ("unexec failed");
#else
	  panic ("systas: unexec unavailable on this platform\n");
#endif
	}
#if 0
      {
	size_t threshold;
	size_t failure_pt;
	size_t in_use;
	size_t high_water_mark;
	int dfa_hits;
	int dfa_misses;
	int dfa_total_hits;
	int dfa_total_misses;

	rx_dfa_cache_statistics (&threshold, &failure_pt, &in_use, &high_water_mark, &dfa_hits, &dfa_misses, &dfa_total_hits, &dfa_total_misses);
	safe_printfmt (2, "dfa cache stats:\n   threshold %lu; failure_pt %lu\n   in_use %lu; high_water_mark %lu\n   hits %d; misses %d; total_hits %d; total_misses %d\n",
		       (unsigned long)threshold,
		       (unsigned long)failure_pt,
		       (unsigned long)in_use,
		       (unsigned long)high_water_mark,
		       dfa_hits, dfa_misses, dfa_total_hits, dfa_total_misses);
      }
      {
	size_t dfa_bytes;
	size_t nfa_bytes;
	size_t threshold;
	size_t failure_pt;
	size_t in_use;
	size_t high_water_mark;
	int nfa_hits;
	int nfa_misses;
	int nfa_saves;

	rx_nfa_cache_statistics (&threshold, &failure_pt, &in_use, &high_water_mark, &nfa_hits, &nfa_misses, &nfa_saves);
	safe_printfmt (2, "nfa cache stats:\n   threshold %lu; failure_pt %lu\n   in_use %lu; high_water_mark %lu\n   hits %d; misses %d; saves %d\n",
		       (unsigned long)threshold,
		       (unsigned long)failure_pt,
		       (unsigned long)in_use,
		       (unsigned long)high_water_mark,
		       nfa_hits, nfa_misses, nfa_saves);

	dfa_bytes = rx_flush_dfa_cache ();
	nfa_bytes = rx_flush_nfa_cache ();
      
	if (dfa_bytes || nfa_bytes)
	  {
	    safe_printfmt (2, "memory retained by dfa cache: %lu bytes\n", (unsigned long)dfa_bytes);
	    safe_printfmt (2, "memory retained by nfa cache: %lu bytes\n", (unsigned long)nfa_bytes);
	  }
      }
#endif
      
      exit (0);
      break;

    case scm_boot_emem:
      {
	static char msg[] = "out of memory\n";
	vu_write (&errn, 2, msg, str_length (msg));
	exit (1);
	break;
      }

    case scm_boot_ereenter:
      {
	static char msg[] = "internal error (bogus reentry warning)\n";
	vu_write (&errn, 2, msg, str_length (msg));
	exit (1);
	break;
      }

    case scm_boot_error:
	{
	  if (!boot_result)
	    boot_result = "scheme error";
	  vu_write (&errn, 2, boot_result, str_length (boot_result));
	  vu_write (&errn, 2, "\n", 1);
	  exit (1);
	}
      break;
    }

  return 0;
}

/* tag: `main' for systas scheme
 */
