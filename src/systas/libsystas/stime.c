/* stime.c - time-related procedures
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


#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include <sys/times.h>
#include <setjmp.h>
#include "hackerlab/bugs/panic.h"
#include "systas/libsystas/system.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/kw.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/throw.h"
#include "systas/libsystas/stime.h"



SCM_PROCEDURE (p_sys_time, "%time");
SCM_PROCEDURE (p_sys_times, "%times");

SCM_KEYWORD (kw_sec, "sec");
SCM_KEYWORD (kw_min, "min");
SCM_KEYWORD (kw_hour, "hour");
SCM_KEYWORD (kw_mday, "mday");
SCM_KEYWORD (kw_mon, "mon");
SCM_KEYWORD (kw_year, "year");
SCM_KEYWORD (kw_wday, "wday");
SCM_KEYWORD (kw_yday, "yday");
SCM_KEYWORD (kw_isdst, "isdst");
SCM_KEYWORD (kw_zone, "zone");
SCM_KEYWORD (kw_gmtoff, "gmtoff");

/* __STDC__ prototypes for static functions */
static long mytime(void);


/************************************************************************
 *(h0 "Time-Related Procedures")
 *
 * The time functions 
 */



#ifdef CLK_TCK
# define CLKTCK CLK_TCK
#else
# ifdef CLOCKS_PER_SEC
#  define CLKTCK CLOCKS_PER_SEC
# else
#  define CLKTCK 60
# endif
#endif



/*(c %time)
 * (%time)
 * 
 * Return the current time of day, measured in seconds beginning at
 * 00:00:00 GMT, January 1, 1970.  May return an errrno object.
 */
SCM_PROC (s_sys_time, "%time", 0, 0, 0, scm_sys_time);
SCM
scm_sys_time ()
{
  SCM_INTS_ENABLED;
  time_t timv;
  SCM answer;

  SCM_DEFER_INTS;
  timv = time((time_t*)0);
  if (timv < 0)
    answer = scm_makerrno (errno);
  else
    answer = scm_ulong2num (timv);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c time)
 * (timevoid)
 * 
 * Return the current time of day, measured in seconds beginning
 * at 00:00:00 GMT, January 1, 1970.
 * 
 * If an error occurs, an exception is signaled.
 */
SCM_PROC(s_time, "time", 0, 0, 0, scm_time);
SCM
scm_time(void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = scm_sys_time ();
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_cons (p_sys_time, SCM_EOL));
  return answer;
}

/*(c localtime)
 * (localtime :optional time)
 * 
 * Return the local time (as from `localtime(3)') in a
 * keyword-argument list.
 * 
 * The list includes keyword/arguments: 
 * 
 * 	:hour <integer>		the current hour (0..23)
 * 	:min <integer>		the current minute (0..59)
 * 	:sec <integer>		the current second (0..59)
 * 	:mday <integer>		the current day/month (1..31)
 * 	:mon <integer>		the current month (0..11)
 * 	:wday <integer>		the current day (0 (sun) .. 6 (sat))
 * 	:yday <integer>		the current day-or-year (0..365)
 * 	:isdst <boolean>	are summer hours in effect?
 * 	:zone <string>		a name for the current timezone
 * 	:gmtoff <integer>	offset from GMT in hours
 * 
 */
SCM_PROC (s_localtime, "localtime", 0, 1, 0, scm_localtime);
SCM
scm_localtime (SCM stime)
{
  static int init = 0;
  long current_time;
  struct tm * now;
  SCM answer;

  if (!init)
    {
      SCM_DEFER_INTS;
      tzset ();
      SCM_ALLOW_INTS;
    }
  if ((SCM_UNDEFINED != stime) && (SCM_BOOL_F != stime))
    current_time = scm_num2long (stime, scm_arg1, s_localtime);
  else
    {
      SCM_DEFER_INTS;
      current_time = time (0);
      SCM_ALLOW_INTS;
    }
  SCM_DEFER_INTS;
  now = localtime (&current_time);
  answer = scm_listify (kw_sec, SCM_MAKINUM (now->tm_sec),
			kw_min, SCM_MAKINUM (now->tm_min),
			kw_hour, SCM_MAKINUM (now->tm_hour),
			kw_mday, SCM_MAKINUM (now->tm_mday),
			kw_mon, SCM_MAKINUM (now->tm_mon),
			kw_year, SCM_MAKINUM (now->tm_year),
			kw_wday, SCM_MAKINUM (now->tm_wday),
			kw_yday, SCM_MAKINUM (now->tm_yday),
			kw_isdst, scm_int_to_bool (now->tm_isdst),
			kw_zone, scm_makfromstr0  (now->tm_zone),
			kw_gmtoff, SCM_MAKINUM (now->tm_gmtoff),
			SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return answer;
}




/*(c %times)
 * (%times)
 * 
 * Return a list of process and system times:
 *
 *	(uptime
 *	 user-time
 * 	 system-time
 *	 children-user-time
 *	 children-system-time)
 *
 * These times are expressed in clock ticks.  The number of clock
 * ticks per second is bound to `internal-time-units-per-second'.
 *
 * See the manual page for `times'.  This function may return
 * an errno object.
 */
SCM_PROC (s_sys_times, "%times", 0, 0, 0, scm_sys_times);
SCM
scm_sys_times (void)
{
  SCM_INTS_ENABLED;
  struct tms tms;
  clock_t uptime;
  SCM answer;


  SCM_DEFER_INTS;
  uptime = times (&tms);
  if (uptime < 0)
    answer = scm_makerrno (errno);
  else
    answer = scm_listify (scm_long2num (uptime),
			  scm_long2num (tms.tms_utime),
			  scm_long2num (tms.tms_stime),
			  scm_long2num (tms.tms_cutime),
			  scm_long2num (tms.tms_cstime),
			  SCM_UNDEFINED);
  SCM_ALLOW_INTS;
  return answer;
}


/*(c times)
 * (times)
 * 
 * Return a list of process and system times:
 *
 *	(uptime
 *	 user-time
 * 	 system-time
 *	 children-user-time
 *	 children-system-time)
 *
 * These times are expressed in clock ticks.  The number of clock
 * ticks per second is bound to `internal-time-units-per-second'.
 *
 * See the manual page for `times'.
 */
SCM_PROC (s_times, "times", 0, 0, 0, scm_times);
SCM
scm_times (void)
{
  SCM_INTS_ENABLED;
  SCM answer;

  answer = scm_sys_times ();
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_cons (p_sys_times, SCM_EOL));
  return answer;
}


/*(c get-internal-run-time)
 * (get-internal-run-timevoid)
 * 
 * Return the amount of time, in "clock ticks", consumed by this
 * process (both user and system).
 *
 * The number of clock ticks per second is bound to
 * `internal-time-units-per-second'.
 */
SCM_PROC(s_get_internal_run_time, "get-internal-run-time", 0, 0, 0, scm_get_internal_run_time);
SCM
scm_get_internal_run_time(void)
{
  SCM_INTS_INDIFFERENT;
  SCM answer;

  answer = mytime ();
  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    answer = SCM_MAKINUM (0);
  return answer;
}


/*(c %gettimeofday)
 * (%gettimeofday)
 * 
 * Return a list:
 * 
 * 	((sec . usec) minuteswest dsttime)
 * 
 * See `gettimeofday'.
 */
SCM_PROC (s_sys_gettimeofday, "%gettimeofday", 0, 0, 0, scm_sys_gettimeofday);
SCM
scm_sys_gettimeofday ()
{
  SCM_INTS_ENABLED;
  struct timeval now;
  struct timezone here;
  int result;
  SCM answer;

  SCM_DEFER_INTS;
  result = gettimeofday (&now, &here);
  if (result < 0)
    answer = scm_makerrno (errno);
  else
    answer = scm_cons (scm_cons (scm_long2num (now.tv_sec), scm_long2num (now.tv_usec)),
		       scm_cons (scm_long2num (here.tz_minuteswest), scm_long2num (here.tz_dsttime)));
  SCM_ALLOW_INTS;

  return answer;
}


struct timeval realtime_base;

/*(c get-internal-real-time)
 * (get-internal-real-timevoid)
 * 
 * Return the amount of real time that have elapsed
 * since this process was started.  The result is expressed
 * in "clock ticks".
 *
 * The number of clock ticks per second is bound to
 * `internal-time-units-per-second'
 */
SCM_PROC(s_get_internal_real_time, "get-internal-real-time", 0, 0, 0, scm_get_internal_real_time);
SCM
scm_get_internal_real_time(void)
{
  SCM_INTS_ENABLED;
  struct timeval now;
  int result;
  SCM answer;

  SCM_DEFER_INTS;
  result = gettimeofday (&now, 0);
  if (result < 0)
    answer = scm_makerrno (errno);
  else
    {
      long seconds;
      long useconds;
      long clicks;

      seconds = now.tv_sec - realtime_base.tv_sec;
      useconds = now.tv_usec - realtime_base.tv_usec;
      clicks = useconds * CLKTCK;
      clicks /= 1000;
      clicks += seconds * CLKTCK;
      answer = scm_long2num (clicks);
    }
  SCM_ALLOW_INTS;

  if (!SCM_IS_IMMEDIATE (answer) && SCM_ERRNOP (answer))
    scm_throw (answer, scm_cons (p_sys_times, SCM_EOL));
  return answer;
}



/*c mytime)
 * static long mytime (void);
 * 
 * Return the amount of time, in "clock ticks", consumed
 * by this process (both user and system).
 * 
 */
static long
mytime(void)
{
  SCM_INTS_NESTED;
  struct tms tms;
  clock_t uptime;
  SCM answer;


  SCM_REDEFER_INTS;
  uptime = times (&tms);
  if (uptime < 0)
    answer = scm_makerrno (errno);
  else
    answer = scm_long2num (tms.tms_utime + tms.tms_stime);
  SCM_REALLOW_INTS;
  return answer;
}




void
scm_init_stime(void)
{
  SCM_INTS_DISABLED;

  scm_intern_symhash("internal-time-units-per-second", SCM_MAKINUM((long)CLKTCK));
  if (gettimeofday (&realtime_base, 0) < 0)
    panic ("unable to get time of day in stime.c");

#include "systas/libsystas/stime.x"
}

