/* numbers.c - numbers
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
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/root.h"


/************************************************************************
 *(h0 "Math Routines")
 * 
 * 
 * These procedures need to be documented in a future releae:
 *
 * 	random32 random32-seed exact?  odd?  even?  abs magnitude *
 * 	quotient remainder modulo gcd lcm logand logior logxor logtest
 * 	logbit?  * logand logior logxor logtest logbit?  lognot
 * 	integer-expt ash * bit-extract logcount integer-length
 * 	number->string * string->number number?  complex?  real?
 * 	rational?  int?  * inexact?  =?  <?  >?  <=?  >=?  zero?
 * 	positive?  negative?  * max min + - / $asinh $acosh $atanh
 * 	truncate round * exact->inexact floor ceiling $sqrt $abs $exp
 * 	$log $sin $cos * $tan $asin $acos $atan $sinh $cosh $tanh
 * 	$expt $atan2 * make-rectangular make-polar real-part imag-part
 * 	angle * inexact->exact truncate
 * 
 * 	< <= = > >= 1+ 1- integer?
 * 	ipow-by-squaring ipow exp expt log sqrt sinh cosh tanh asinh
 * 	acosh atanh sin cos tan asin acos atan

 */



/* MAXEXP is the maximum double precision expontent
 * FLTMAX is less than or scm_equal the largest single precision float
 */

#ifdef SCM_FLOATS
# include <float.h>
# ifdef DBL_MAX_10_EXP
#  define MAXEXP DBL_MAX_10_EXP
# else
#  define MAXEXP 308   /* IEEE doubles */
# endif /* def DBL_MAX_10_EXP */
# ifdef FLT_MAX
#  define FLTMAX FLT_MAX
# else
#  define FLTMAX 1e+23
# endif /* def FLT_MAX */
#endif /* def SCM_FLOATS */



SCM_SYMBOL (s_bignum, "bignum");
SCM_SYMBOL (s_adjbig, "scm_adjbig");
SCM_SYMBOL (s_complex, "complex");
SCM_SYMBOL (s_real, "real");
SCM_SYMBOL (s_dbl2big, "dbl2big");
static SCM s_abs;



#define DIGITS '0':case '1':case '2':case '3':case '4':\
 		case '5':case '6':case '7':case '8':case '9'


/* IS_INF tests its floating point number for infiniteness
 */
#ifndef IS_INF
# define IS_INF(x) ((x)==(x)/2)
#endif



/*s
 * random32
 * 
 * Return a pseudo-random integer in the range 0..2^32
 */
SCM_PROC (s_random32, "random32", 0, 0, 0, scm_random32);
SCM
scm_random32 ()
{
  SCM_INTS_ENABLED;
  unsigned long r;

  SCM_DEFER_INTS;
  r = random ();
  SCM_ALLOW_INTS;
  return scm_ulong2num (r);
}


/*s
 * random32-seed seed
 * 
 * Set the state of the pseudo-random number generator `random32'.
 * Setting the state to the same value twice causes the generator to
 * return the same sequence of numbers.
 */
SCM_PROC (s_random32_seed, "random32-seed", 1, 0, 0, scm_random32_seed);
SCM
scm_random32_seed (SCM seed)
{
  SCM_INTS_ENABLED;
  unsigned long s;

  s = scm_num2ulong (seed, scm_arg1, s_random32_seed);
  SCM_DEFER_INTS;
  srandom ((unsigned int)s);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}



/*s
 * exact? obj
 * 
 * Return #t of `obj' is an exact number, #f otherwise.
 */
SCM_PROC(s_exact_p, "exact?", 1, 0, 0, scm_exact_p);
SCM
scm_exact_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  if (SCM_INUMP(x))
    return SCM_BOOL_T;
#ifdef SCM_BIGDIG
  if (!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x))
    return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}


/*s
 * odd? obj
 * 
 * Return #t of `obj' is an odd number #f otherwise.
 */
SCM_PROC(s_odd_p, "odd?", 1, 0, 0, scm_odd_p);
SCM
scm_odd_p(SCM n)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP (n))
    return (4 & (int)n) ? SCM_BOOL_T : SCM_BOOL_F;

  if (!SCM_NINUMP(n))
    return SCM_BOOL_F;

  if (SCM_INEXP (n))
    {
      double integer;
      double fraction;
      int even;
      fraction = modf (SCM_REAL (n), &integer);
      even = (   (fraction == 0.0)
	      && !((long)integer & 1));
      return scm_int_to_bool (!even);
    }
#ifdef SCM_BIGDIG
  if (SCM_BIGP(n))
    return scm_int_to_bool (1 & SCM_BDIGITS(n)[0]);
#endif

  SCM_ASSERT (0, n, scm_arg1, s_odd_p);
  return SCM_BOOL_F;		/* not reached */
}


/*s
 * even?
 * 
 * Return #t of `obj' is an even number #f otherwise.
 */
SCM_PROC(s_even_p, "even?", 1, 0, 0, scm_even_p);
SCM
scm_even_p(SCM n)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP (n))
    return (4 & (int)n) ? SCM_BOOL_F : SCM_BOOL_T;

  if (!SCM_NINUMP(n))
    return SCM_BOOL_F;

  if (SCM_INEXP (n))
    {
      double integer;
      double fraction;
      int even;
      fraction = modf (SCM_REAL (n), &integer);
      even = (   (fraction == 0.0)
	      && !((long)integer & 1));
      return scm_int_to_bool (even);
    }
#ifdef SCM_BIGDIG
  if (SCM_BIGP(n))
    return scm_int_to_bool (!(1 & SCM_BDIGITS(n)[0]));
#endif

  SCM_ASSERT (0, n, scm_arg1, s_even_p);
  return SCM_BOOL_F;		/* not reached */
}


/* scm_abs
 * 
 * This is not the Scheme procedure `abs', thought it is
 * used to implement it.  See scm_magnitude.
 */
static SCM
scm_abs(SCM x)
{
  SCM_INTS_ENABLED;

#ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_abs);
      if (SCM_TYP16(x)==scm_tc16_bigpos) return x;
      return scm_copybig(x, 0);
    }
#else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_abs);
#endif
  if (SCM_INUM(x) >= 0) return x;
  x = -SCM_INUM(x);
  if (!SCM_POSSCM_FIXABLE(x))
#ifdef SCM_BIGDIG
    return scm_long2big(x);
#else
  scm_wta(SCM_MAKINUM(-x), (char *)scm_ovflow, s_abs);
#endif
  return SCM_MAKINUM(x);
}

/*s
 * abs n
 * 
 * Return the absolute value of `n'.
 */
/*s
 * magnitude n
 * 
 * Return the absolute value of `n'.
 */
SCM_PROC(s_abs, "abs", 1, 0, 0, scm_magnitude);
SCM_PROC(s_magnitude, "magnitude", 1, 0, 0, scm_magnitude);
SCM
scm_magnitude(SCM z)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP(z))
    return scm_abs(z);

# ifdef SCM_BIGDIG
  if (!!SCM_IS_IMMEDIATE(z))
    goto badz;
  if (SCM_BIGP(z))
    return scm_abs(z);
#  ifndef SCM_RECKLESS
  if (!(SCM_INEXP(z)))
    {
    badz:
      scm_wta(z, scm_arg1, s_magnitude);
    }
#  endif
# else
  SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_INEXP(z), z, scm_arg1, s_magnitude);
# endif

  if (SCM_CPLXP(z))
    {
      double i;
      double r;
      double s;
      i = SCM_IMAG(z);
      r = SCM_REAL(z);
      SCM_DEFER_INTS;
      s = sqrt(i*i+r*r);
      SCM_ALLOW_INTS;
      return scm_makdbl(s, 0.0);
    }

  {
    double f;
    SCM_DEFER_INTS;
    f = fabs(SCM_REALPART(z));
    SCM_ALLOW_INTS;
    return scm_makdbl(f, 0.0);
  }
}


SCM_PROC(s_quotient, "quotient", 2, 0, 0, scm_quotient);
SCM
scm_quotient(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  long z;
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      long w;
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_quotient);
      if (SCM_NINUMP(y))
	{
	  if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	    goto bady;
	  return scm_divbigbig(SCM_BDIGITS(x),
			       SCM_NUMDIGS(x),
			       SCM_BDIGITS(y),
			       SCM_NUMDIGS(y),
			       SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y),
			       2);
	}
      z = SCM_INUM(y);
      if (!z)
	goto ov;
      if (1==z) return x;
      if (z < 0) z = -z;
      if (z < SCM_BIGRAD) {
			    w = scm_copybig(x, SCM_BIGSIGN(x) ? (y>0) : (y<0));
			    scm_divbigdig(SCM_BDIGITS(w), SCM_NUMDIGS(w), (SCM_BIGDIG)z);
			    return scm_normbig(w);
			  }
#ifndef SCM_DIGSTOOBIG
      w = scm_pseudolong(z);
      return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), (SCM_BIGDIG *)&w, SCM_DIGSPERLONG,
			   SCM_BIGSIGN(x) ? (y>0) : (y<0), 2);
#else
      { SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	scm_longdigs(z, zdigs);
	return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), zdigs, SCM_DIGSPERLONG,
			     SCM_BIGSIGN(x) ? (y>0) : (y<0), 2);
      }
#endif
    }
  if (SCM_NINUMP(y))
    {
# ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_quotient);
# endif
      return SCM_INUM0;
    }
#else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_quotient);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_quotient);
#endif
  if ((z = SCM_INUM(y))==0)
    ov: scm_wta(y, scm_ovflow, s_quotient);
  z = SCM_INUM(x)/z;
#ifdef BADIVSGNS
  {
#if (__TURBOC__==1)
    long t = ((y<0) ? -SCM_INUM(x) : SCM_INUM(x))%SCM_INUM(y);
#else
    long t = SCM_INUM(x)%SCM_INUM(y);
#endif
    if (t==0) ;
    else if (t < 0)
      if (x < 0) ;
      else z--;
    else if (x < 0) z++;
  }
#endif
  if (!SCM_FIXABLE(z))
#ifdef SCM_BIGDIG
    return scm_long2big(z);
#else
  scm_wta(x, scm_ovflow, s_quotient);
#endif
  return SCM_MAKINUM(z);
}


SCM_PROC(s_remainder, "remainder", 2, 0, 0, scm_remainder);
SCM
scm_remainder(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  long z;
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_remainder);
      if (SCM_NINUMP(y))
	{
	  if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	    goto bady;
	  return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			       SCM_BIGSIGN(x), 0);
	}
      z = SCM_INUM(y);
      if (!z)
	goto ov;
      return scm_divbigint(x, z, SCM_BIGSIGN(x), 0);
    }
  if (SCM_NINUMP(y))
    {
# ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_remainder);
# endif
      return x;
    }
#else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_remainder);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_remainder);
#endif
  if (!(z = SCM_INUM(y)))
    ov: scm_wta(y, scm_ovflow, s_remainder);
#if (__TURBOC__==1)
  if (z < 0) z = -z;
#endif
  z = SCM_INUM(x)%z;
#ifdef BADIVSGNS
  if (!z) ;
  else if (z < 0)
    if (x < 0) ;
    else z += SCM_INUM(y);
  else if (x < 0) z -= SCM_INUM(y);
#endif
  return SCM_MAKINUM(z);
}


SCM_PROC(s_modulo, "modulo", 2, 0, 0, scm_modulo);
SCM
scm_modulo(SCM x, SCM y)
{
  SCM_INTS_ENABLED;
  long yy, z;
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_modulo);
      if (SCM_NINUMP(y))
	{
	  if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	    goto bady;
	  return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			       SCM_BIGSIGN(y), (SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y)) ? 1 : 0);
	}
      z = SCM_INUM(y);
      if (!z)
	goto ov;
      return scm_divbigint(x, z, y < 0, (SCM_BIGSIGN(x) ? (y > 0) : (y < 0)) ? 1 : 0);
    }
  if (SCM_NINUMP(y))
    {
# ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_modulo);
# endif
      return (SCM_BIGSIGN(y) ? (x>0) : (x<0)) ? scm_sum(x, y) : x;
    }
#else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_modulo);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_modulo);
#endif
  if (!(yy = SCM_INUM(y)))
    ov: scm_wta(y, scm_ovflow, s_modulo);
#if (__TURBOC__==1)
  z = SCM_INUM(x);
  z = ((yy<0) ? -z : z)%yy;
#else
  z = SCM_INUM(x)%yy;
#endif
  return SCM_MAKINUM(((yy<0) ? (z>0) : (z<0)) ? z+yy : z);
}


SCM_PROC1 (s_gcd, "gcd", scm_tc7_asubr, scm_gcd);
SCM
scm_gcd(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  long u, v, k, t;
  if (SCM_UNBNDP(y))
    return (SCM_UNBNDP(x)
	    ? SCM_INUM0
	    : x);
 tailrec:
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
    big_gcd:
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_gcd);
      if (SCM_BIGSIGN(x))
	x = scm_copybig(x, 0);
    newy:
      if (SCM_NINUMP(y))
	{
	  SCM_ASSERT(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y), y, scm_arg2, s_gcd);
	  if (SCM_BIGSIGN(y))
	    y = scm_copybig(y, 0);
	  switch (scm_bigcomp(x, y))
	    {
	    case -1:
	    swaprec: t = scm_remainder(x, y); x = y; y = t; goto tailrec;
	    case  0: return x;
	    case  1: y = scm_remainder(y, x); goto newy;
	    }
	  /* instead of the switch, we could just return scm_gcd(y, scm_modulo(x, y)); */
	}
      if (SCM_INUM0==y) return x; goto swaprec;
    }
  if (SCM_NINUMP(y))
    {
      t=x;
      x=y;
      y=t;
      goto big_gcd;
    }
#else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_gcd);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_gcd);
#endif
  u = SCM_INUM(x);
  if (u<0) u = -u;
  v = SCM_INUM(y);
  if (v<0) v = -v;
  else if (0==v) goto getout;
  if (0==u) {u = v; goto getout;}
  for (k = 1;!(1 & ((int)u|(int)v));k <<= 1, u >>= 1, v >>= 1);
  if (1 & (int)u) t = -v;
  else {
	 t = u;
       b3:
	 t = SCM_SRS(t, 1);
       }
  if (!(1 & (int)t)) goto b3;
  if (t>0) u = t;
  else v = -t;
  if ((t = u-v)) goto b3;
  u = u*k;
 getout:
  if (!SCM_POSSCM_FIXABLE(u))
#ifdef SCM_BIGDIG
    return scm_long2big(u);
#else
  scm_wta(x, scm_ovflow, s_gcd);
#endif
  return SCM_MAKINUM(u);
}


SCM_PROC1 (s_lcm, "lcm", scm_tc7_asubr, scm_lcm);
SCM
scm_lcm(SCM n1, SCM n2)
{
  SCM_INTS_ENABLED;

  SCM d;
  if (SCM_UNBNDP(n2))
    {
      n2 = SCM_MAKINUM(1L);
      if (SCM_UNBNDP(n1))
	return n2;
    }
  d = scm_gcd(n1, n2);
  if (SCM_INUM0==d)
    return d;
  return scm_abs(scm_product(n1, scm_quotient(n2, d)));
}


#ifndef SCM_BIGDIG
# ifndef SCM_FLOATS
#  define scm_long2num SCM_MAKINUM
# endif
#endif

#ifndef scm_long2num
SCM_PROC1 (s_logand, "logand", scm_tc7_asubr, scm_logand);
SCM
scm_logand(SCM n1, SCM n2)
{
  SCM_INTS_ENABLED;

  return scm_long2num(scm_num2long(n1, scm_arg1, s_logand)
		      & scm_num2long(n2, scm_arg2, s_logand));
}

SCM_PROC1 (s_logior, "logior", scm_tc7_asubr, scm_logior);
SCM
scm_logior(SCM n1, SCM n2)
{
  SCM_INTS_ENABLED;

  return scm_long2num(scm_num2long(n1, scm_arg1, s_logior)
		      | scm_num2long(n2, scm_arg2, s_logior));
}

SCM_PROC1 (s_logxor, "logxor", scm_tc7_asubr, scm_logxor);
SCM
scm_logxor(SCM n1, SCM n2)
{
  SCM_INTS_ENABLED;

  return scm_long2num(scm_num2long(n1, scm_arg1, s_logxor)
		      ^ scm_num2long(n2, scm_arg2, s_logxor));
}

SCM_PROC(s_logtest, "logtest", 2, 0, 0, scm_logtest);
SCM
scm_logtest(SCM n1, SCM n2)
{
  SCM_INTS_ENABLED;

  return ((scm_num2long (n1, scm_arg1, s_logtest)
	   & scm_num2long (n2, scm_arg2, s_logtest))
	  ? SCM_BOOL_T : SCM_BOOL_F);
}


SCM_PROC(s_logbit_p, "logbit?", 2, 0, 0, scm_logbit_p);
SCM
scm_logbit_p(SCM n1, SCM n2)
{
  SCM_INTS_ENABLED;

  return (((1 << scm_num2long (n1, scm_arg1, s_logtest))
	   & scm_num2long (n2, scm_arg2, s_logtest))
	  ? SCM_BOOL_T : SCM_BOOL_F);
}

#else

SCM_PROC1 (s_logand, "logand", scm_tc7_asubr, scm_logand);
SCM
scm_logand(SCM n1, SCM n2)
{
  SCM_ASSERT(SCM_INUMP(n1), n1, scm_arg1, s_logand);
  SCM_ASSERT(SCM_INUMP(n2), n2, scm_arg2, s_logand);
  return SCM_MAKINUM(SCM_INUM(n1) & SCM_INUM(n2));
}

SCM_PROC1 (s_logior, "logior", scm_tc7_asubr, scm_logior);
SCM
scm_logior(SCM n1, SCM n2)
{
  SCM_ASSERT(SCM_INUMP(n1), n1, scm_arg1, s_logior);
  SCM_ASSERT(SCM_INUMP(n2), n2, scm_arg2, s_logior);
  return SCM_MAKINUM(SCM_INUM(n1) | SCM_INUM(n2));
}

SCM_PROC1 (s_logxor, "logxor", scm_tc7_asubr, scm_logxor);
SCM
scm_logxor(SCM n1, SCM n2)
{
  SCM_ASSERT(SCM_INUMP(n1), n1, scm_arg1, s_logxor);
  SCM_ASSERT(SCM_INUMP(n2), n2, scm_arg2, s_logxor);
  return SCM_MAKINUM(SCM_INUM(n1) ^ SCM_INUM(n2));
}

SCM_PROC(s_logtest, "logtest", 2, 0, 0, scm_logtest);
SCM
scm_logtest(SCM n1, SCM n2)
{
  SCM_ASSERT(SCM_INUMP(n1), n1, scm_arg1, s_logtest);
  SCM_ASSERT(SCM_INUMP(n2), n2, scm_arg2, s_logtest);
  return (SCM_INUM(n1) & SCM_INUM(n2)) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_PROC(s_logbit_p, "logbit?", 2, 0, 0, scm_logbit_p);
SCM
scm_logbit_p(SCM n1, SCM n2)
{
  SCM_ASSERT(SCM_INUMP(n1) && SCM_INUM(n1) >= 0, n1, scm_arg1, s_logbit_p);
  SCM_ASSERT(SCM_INUMP(n2), n2, scm_arg2, s_logbit_p);
  return ((1 << SCM_INUM(n1)) & SCM_INUM(n2)) ? SCM_BOOL_T : SCM_BOOL_F;
}
#endif

SCM_PROC(s_lognot, "lognot", 1, 0, 0, scm_lognot);
SCM
scm_lognot(SCM n)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(SCM_INUMP(n), n, scm_arg1, s_lognot);
  return scm_difference(SCM_MAKINUM(-1L), n);
}

SCM_PROC(s_integer_expt, "integer-expt", 2, 0, 0, scm_integer_expt);
SCM
scm_integer_expt(SCM z1, SCM z2)
{
  SCM_INTS_ENABLED;

  SCM acc = SCM_MAKINUM(1L);
#ifdef SCM_BIGDIG
  if (SCM_INUM0==z1 || acc==z1) return z1;
  else if (SCM_MAKINUM(-1L)==z1) return SCM_BOOL_F==scm_even_p(z2)?z1:acc;
#endif
  SCM_ASSERT(SCM_INUMP(z2), z2, scm_arg2, s_integer_expt);
  z2 = SCM_INUM(z2);
  if (z2 < 0) {
    z2 = -z2;
    z1 = scm_divide(z1, SCM_UNDEFINED);
  }
  while(1) {
    if (0==z2) return acc;
    if (1==z2) return scm_product(acc, z1);
    if (z2 & 1) acc = scm_product(acc, z1);
    z1 = scm_product(z1, z1);
    z2 >>= 1;
  }
}

SCM_PROC(s_ash, "ash", 2, 0, 0, scm_ash);
SCM
scm_ash(SCM n, SCM cnt)
{
  SCM_INTS_ENABLED;

  SCM res = SCM_INUM(n);
  SCM_ASSERT(SCM_INUMP(cnt), cnt, scm_arg2, s_ash);
#ifdef SCM_BIGDIG
  if(cnt < 0) {
    res = scm_integer_expt(SCM_MAKINUM(2), SCM_MAKINUM(-SCM_INUM(cnt)));
    if (SCM_BOOL_F != scm_negative_p(n))
      return scm_sum(SCM_MAKINUM(-1L), scm_quotient(scm_sum(SCM_MAKINUM(1L), n), res));
    else return scm_quotient(n, res);
  }
  else return scm_product(n, scm_integer_expt(SCM_MAKINUM(2), cnt));
#else
  SCM_ASSERT(SCM_INUMP(n), n, scm_arg1, s_ash);
  cnt = SCM_INUM(cnt);
  if (cnt < 0) return SCM_MAKINUM(SCM_SRS(res, -cnt));
  res = SCM_MAKINUM(res<<cnt);
  if (SCM_INUM(res)>>cnt != SCM_INUM(n)) scm_wta(n, scm_ovflow, s_ash);
  return res;
#endif
}

SCM_PROC(s_bit_extract, "bit-extract", 3, 0, 0, scm_bit_extract);
SCM
scm_bit_extract(SCM n, SCM start, SCM end)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(SCM_INUMP(start), start, scm_arg2, s_bit_extract);
  SCM_ASSERT(SCM_INUMP(end), end, scm_arg3, s_bit_extract);
  start = SCM_INUM(start); end = SCM_INUM(end);
  SCM_ASSERT(end >= start, SCM_MAKINUM(end), scm_outofrange, s_bit_extract);
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(n))
    return
      scm_logand(scm_difference(scm_integer_expt(SCM_MAKINUM(2), SCM_MAKINUM(end - start)),
				SCM_MAKINUM(1L)),
		 scm_ash(n, SCM_MAKINUM(-start)));
#else
  SCM_ASSERT(SCM_INUMP(n), n, scm_arg1, s_bit_extract);
#endif
  return SCM_MAKINUM((SCM_INUM(n)>>start) & ((1L<<(end-start))-1));
}

char scm_logtab[] = {0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};
SCM_PROC(s_logcount, "logcount", 1, 0, 0, scm_logcount);
SCM
scm_logcount (SCM n)
{
  SCM_INTS_ENABLED;

  unsigned long c = 0;
  long nn;
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(n))
    {
      size_t i; SCM_BIGDIG *ds, d;
      SCM_ASSERT(!SCM_IS_IMMEDIATE(n) && SCM_BIGP(n), n, scm_arg1, s_logcount);
      if (SCM_BIGSIGN(n))
	return scm_logcount(scm_difference(SCM_MAKINUM(-1L), n));
      ds = SCM_BDIGITS(n);
      for(i = SCM_NUMDIGS(n); i--; )
	for(d = ds[i]; d; d >>= 4) c += scm_logtab[15 & d];
      return SCM_MAKINUM(c);
    }
#else
  SCM_ASSERT(SCM_INUMP(n), n, scm_arg1, s_logcount);
#endif
  if ((nn = SCM_INUM(n)) < 0) nn = -1 - nn;
  for(; nn; nn >>= 4) c += scm_logtab[15 & nn];
  return SCM_MAKINUM(c);
}

char scm_ilentab[] = {0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4};
SCM_PROC(s_integer_length, "integer-length", 1, 0, 0, scm_integer_length);
SCM
scm_integer_length(SCM n)
{
  SCM_INTS_ENABLED;

  unsigned long c = 0;
  long nn;
  unsigned int l = 4;
#ifdef SCM_BIGDIG
  if (SCM_NINUMP(n))
    {
      SCM_BIGDIG *ds, d;
      SCM_ASSERT(!SCM_IS_IMMEDIATE(n) && SCM_BIGP(n), n, scm_arg1, s_integer_length);
      if (SCM_BIGSIGN(n))
	return scm_integer_length(scm_difference(SCM_MAKINUM(-1L), n));
      ds = SCM_BDIGITS(n);
      d = ds[c = SCM_NUMDIGS(n)-1];
      for(c *= SCM_BITSPERDIG; d; d >>= 4) {c += 4; l = scm_ilentab[15 & d];}
      return SCM_MAKINUM(c - 4 + l);
    }
#else
  SCM_ASSERT(SCM_INUMP(n), n, scm_arg1, s_integer_length);
#endif
  if ((nn = SCM_INUM(n)) < 0) nn = -1 - nn;
  for(;nn; nn >>= 4) {c += 4; l = scm_ilentab[15 & nn];}
  return SCM_MAKINUM(c - 4 + l);
}


#ifdef SCM_BIGDIG

SCM
scm_mkbig(size_t nlen, int sign)
{
  SCM_INTS_NESTED;

  SCM v = nlen;
  if (((v << 16) >> 16) != nlen)
    scm_panic ("bignum too large");
  SCM_NEWCELL(v);
  SCM_REDEFER_INTS;
  SCM_CDR (v) = (SCM)scm_must_malloc((long)(nlen*sizeof(SCM_BIGDIG)));
  SCM_SETNUMDIGS(v, nlen, sign?scm_tc16_bigneg:scm_tc16_bigpos);
  SCM_REALLOW_INTS;
  return v;
}

SCM
scm_big2inum(SCM b, size_t l)
{
  SCM_INTS_INDIFFERENT;

  unsigned long num = 0;
  SCM_BIGDIG *tmp = SCM_BDIGITS(b);
  while (l--) num = SCM_BIGUP(num) + tmp[l];
  if (SCM_TYP16(b)==scm_tc16_bigpos) {
    if (SCM_POSSCM_FIXABLE(num))
      return SCM_MAKINUM(num);
  }
  else if (SCM_UNEGSCM_FIXABLE(num))
    return SCM_MAKINUM(-num);
  return b;
}


SCM
scm_adjbig(SCM b, size_t nlen)
{
  SCM_INTS_ENABLED;

  long nsiz = nlen;
  if (((nsiz << 16) >> 16) != nlen) scm_wta(SCM_MAKINUM(nsiz), scm_nalloc, s_adjbig);
  SCM_DEFER_INTS;
  SCM_CDR(b) = (SCM)scm_must_realloc((char *)SCM_CDR(b),
				     (long)(SCM_NUMDIGS(b)*sizeof(SCM_BIGDIG)),
				     (long)(nsiz*sizeof(SCM_BIGDIG)));
  SCM_SETNUMDIGS(b, nsiz, SCM_TYP16(b));
  SCM_ALLOW_INTS;
  return b;
}


SCM
scm_normbig(SCM b)
{
  SCM_INTS_ENABLED;

#ifndef _UNICOS  
  size_t nlen = SCM_NUMDIGS(b);
#else
  int nlen = SCM_NUMDIGS(b);	/* unsigned nlen breaks on Cray when nlen => 0 */
#endif
  SCM_BIGDIG *zds = SCM_BDIGITS(b);
  while (nlen-- && !zds[nlen]); nlen++;
  if (nlen * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM))
    if (SCM_INUMP(b = scm_big2inum(b, (size_t)nlen)))
      return b;
  if (SCM_NUMDIGS(b)==nlen) return b;
  return scm_adjbig(b, (size_t)nlen);
}


SCM
scm_copybig(SCM b, int sign)
{
  SCM_INTS_ENABLED;

  size_t i = SCM_NUMDIGS(b);
  SCM ans = scm_mkbig(i, sign);
  SCM_BIGDIG *src = SCM_BDIGITS(b), *dst = SCM_BDIGITS(ans);
  while (i--) dst[i] = src[i];
  return ans;
}


SCM
scm_long2big(long n)
{
  SCM_INTS_NESTED;

  size_t i = 0;
  SCM_BIGDIG *digits;
  SCM ans = scm_mkbig(SCM_DIGSPERLONG, n<0);
  digits = SCM_BDIGITS(ans);
  if (n < 0) n = -n;
  while (i < SCM_DIGSPERLONG) {
    digits[i++] = SCM_BIGLO(n);
    n = SCM_BIGDN((unsigned long)n);
  }
  return ans;
}


SCM
scm_2ulong2big(unsigned long * np)
{
  SCM_INTS_ENABLED;

  unsigned long n;
  size_t i;
  SCM_BIGDIG *digits;
  SCM ans;

  ans = scm_mkbig(2 * SCM_DIGSPERLONG, 0);
  digits = SCM_BDIGITS(ans);

  n = np[0];
  for (i = 0; i < SCM_DIGSPERLONG; ++i)
    {
      digits[i] = SCM_BIGLO(n);
      n = SCM_BIGDN((unsigned long)n);
    }
  n = np[1];
  for (i = 0; i < SCM_DIGSPERLONG; ++i)
    {
      digits[i + SCM_DIGSPERLONG] = SCM_BIGLO(n);
      n = SCM_BIGDN((unsigned long)n);
    }
  return ans;
}


SCM
scm_ulong2big(unsigned long n)
{
  SCM_INTS_ENABLED;

  size_t i = 0;
  SCM_BIGDIG *digits;
  SCM ans = scm_mkbig(SCM_DIGSPERLONG, 0);
  digits = SCM_BDIGITS(ans);
  while (i < SCM_DIGSPERLONG) {
    digits[i++] = SCM_BIGLO(n);
    n = SCM_BIGDN(n);
  }
  return ans;
}


int
scm_bigcomp(SCM x, SCM y)
{
  SCM_INTS_INDIFFERENT;

  int xsign = SCM_BIGSIGN(x);
  int ysign = SCM_BIGSIGN(y);
  size_t xlen, ylen;
  if (ysign < xsign) return 1;
  if (ysign > xsign) return -1;
  if ((ylen = SCM_NUMDIGS(y)) > (xlen = SCM_NUMDIGS(x))) return (xsign) ? -1 : 1;
  if (ylen < xlen) return (xsign) ? 1 : -1;
  while(xlen-- && (SCM_BDIGITS(y)[xlen]==SCM_BDIGITS(x)[xlen]));
  if (-1==xlen) return 0;
  return (SCM_BDIGITS(y)[xlen] > SCM_BDIGITS(x)[xlen]) ?
    (xsign ? -1 : 1) : (xsign ? 1 : -1);
}

#ifndef SCM_DIGSTOOBIG

long
scm_pseudolong(long x)
{
  SCM_INTS_INDIFFERENT;

  union {
    long l;
    SCM_BIGDIG bd[SCM_DIGSPERLONG];
  } p;
  size_t i = 0;
  if (x < 0) x = -x;
  while (i < SCM_DIGSPERLONG) {p.bd[i++] = SCM_BIGLO(x); x = SCM_BIGDN(x);}
  /*  p.bd[0] = SCM_BIGLO(x); p.bd[1] = SCM_BIGDN(x); */
  return p.l;
}

#else

void
scm_longdigs(long x, SCM_BIGDIG digs[])
{
  size_t i = 0;
  if (x < 0) x = -x;
  while (i < SCM_DIGSPERLONG) {digs[i++] = SCM_BIGLO(x); x = SCM_BIGDN(x);}
}
#endif


SCM
scm_addbig(SCM_BIGDIG *x, size_t nx, int xsgn, SCM bigy, int sgny)
{
  SCM_INTS_ENABLED;

  /* Assumes nx <= SCM_NUMDIGS(bigy) */
  /* Assumes xsgn and sgny scm_equal either 0 or 0x0100 */
  long num = 0;
  size_t i = 0, ny = SCM_NUMDIGS(bigy);
  SCM z = scm_copybig(bigy, SCM_BIGSIGN(bigy) ^ sgny);
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  if (xsgn ^ SCM_BIGSIGN(z)) {
    do {
      num += (long) zds[i] - x[i];
      if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
      else {zds[i] = SCM_BIGLO(num); num = 0;}
    } while (++i < nx);
    if (num && nx==ny) {
      num = 1; i = 0;
      SCM_CAR(z) ^= 0x0100;
      do {
	num += (SCM_BIGRAD-1) - zds[i];
	zds[i++] = SCM_BIGLO(num);
	num = SCM_BIGDN(num);
      } while (i < ny);
    }
    else while (i < ny) {
      num += zds[i];
      if (num < 0) {zds[i++] = num + SCM_BIGRAD; num = -1;}
      else {zds[i++] = SCM_BIGLO(num); num = 0;}
    }
  } else {
    do {
      num += (long) zds[i] + x[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
    } while (i < nx);
    if (!num) return z;
    while (i < ny) {
      num += zds[i];
      zds[i++] = SCM_BIGLO(num);
      num = SCM_BIGDN(num);
      if (!num) return z;
    }
    if (num) {z = scm_adjbig(z, ny+1); SCM_BDIGITS(z)[ny] = num; return z;}
  }
  return scm_normbig(z);
}

SCM
scm_mulbig(SCM_BIGDIG *x, size_t nx, SCM_BIGDIG *y, size_t ny, int sgn)
{
  SCM_INTS_ENABLED;

  size_t i = 0, j = nx + ny;
  unsigned long n = 0;
  SCM z = scm_mkbig(j, sgn);
  SCM_BIGDIG *zds = SCM_BDIGITS(z);
  while (j--) zds[j] = 0;
  do {
    j = 0;
    if (x[i]) {
      do {
	n += zds[i + j] + ((unsigned long) x[i] * y[j]);
	zds[i + j++] = SCM_BIGLO(n);
	n = SCM_BIGDN(n);
      } while (j < ny);
      if (n) {zds[i + j] = n; n = 0;}
    }
  } while (++i < nx);
  return scm_normbig(z);
}

unsigned int
scm_divbigdig(SCM_BIGDIG *ds, size_t h, SCM_BIGDIG dv)
{
  SCM_INTS_INDIFFERENT;

  unsigned long t2 = 0;
  while(h--) {
    t2 = SCM_BIGUP(t2) + ds[h];
    ds[h] = t2 / dv;
    t2 %= dv;
  }
  return t2;
}


SCM
scm_divbigint(SCM x, long z, int sgn, int mode)
{
  SCM_INTS_ENABLED;

  if (z < 0) z = -z;
  if (z < SCM_BIGRAD) {
    unsigned long t2 = 0;
    SCM_BIGDIG *ds = SCM_BDIGITS(x);
    size_t nd = SCM_NUMDIGS(x);
    while(nd--) t2 = (SCM_BIGUP(t2) + ds[nd]) % z;
    if (mode && t2) t2 = z - t2;
    return SCM_MAKINUM(sgn ? -t2 : t2);
  }
  {
#ifndef SCM_DIGSTOOBIG
    unsigned long t2 = scm_pseudolong(z);
    return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), (SCM_BIGDIG *)&t2,
			 SCM_DIGSPERLONG, sgn, mode); 
#else
    SCM_BIGDIG t2[SCM_DIGSPERLONG];
    scm_longdigs(z, t2);
    return scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), t2, SCM_DIGSPERLONG, sgn, mode);
#endif
  }
}

SCM
scm_divbigbig(SCM_BIGDIG *x, size_t nx, SCM_BIGDIG *y, size_t ny, int sgn, int modes)
{
  SCM_INTS_ENABLED;

  /* modes description
     0	remainder
     1	scm_modulo
     2	quotient
     3	quotient but returns 0 if division is not exact. */
  size_t i = 0, j = 0;
  long num = 0;
  unsigned long t2 = 0;
  SCM z, newy;
  SCM_BIGDIG  d = 0, qhat, *zds, *yds;
  /* algorithm requires nx >= ny */
  if (nx < ny)
    switch (modes) {
    case 0:			/* remainder -- just return x */
      z = scm_mkbig(nx, sgn); zds = SCM_BDIGITS(z);
      do {zds[i] = x[i];} while (++i < nx);
      return z;
    case 1:			/* scm_modulo -- return y-x */
      z = scm_mkbig(ny, sgn); zds = SCM_BDIGITS(z);
      do {
	num += (long) y[i] - x[i];
	if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
	else {zds[i] = num; num = 0;}
      } while (++i < nx);
      while (i < ny) {
	num += y[i];
	if (num < 0) {zds[i++] = num + SCM_BIGRAD; num = -1;}
	else {zds[i++] = num; num = 0;}
      }
      goto doadj;
    case 2: return SCM_INUM0;	/* quotient is zero */
    case 3: return 0;		/* the division is not exact */
    }

  z = scm_mkbig(nx==ny ? nx+2 : nx+1, sgn); zds = SCM_BDIGITS(z);
  if (nx==ny) zds[nx+1] = 0;
  while(!y[ny-1]) ny--;		/* in case y came in as a psuedolong */
  if (y[ny-1] < (SCM_BIGRAD>>1)) {  /* normalize operands */
    d = SCM_BIGRAD/(y[ny-1]+1);
    newy = scm_mkbig(ny, 0); yds = SCM_BDIGITS(newy);
    while(j < ny)
      {t2 += (unsigned long) y[j]*d; yds[j++] = SCM_BIGLO(t2); t2 = SCM_BIGDN(t2);}
    y = yds; j = 0; t2 = 0;
    while(j < nx)
      {t2 += (unsigned long) x[j]*d; zds[j++] = SCM_BIGLO(t2); t2 = SCM_BIGDN(t2);}
    zds[j] = t2;
  }
  else {zds[j = nx] = 0; while (j--) zds[j] = x[j];}
  j = nx==ny ? nx+1 : nx;	/* dividend needs more digits than divisor */
  do {				/* loop over digits of quotient */
    if (zds[j]==y[ny-1]) qhat = SCM_BIGRAD-1;
    else qhat = (SCM_BIGUP(zds[j]) + zds[j-1])/y[ny-1];
    if (!qhat) continue;
    i = 0; num = 0; t2 = 0;
    do {			/* multiply and subtract */
      t2 += (unsigned long) y[i] * qhat;
      num += zds[j - ny + i] - SCM_BIGLO(t2);
      if (num < 0) {zds[j - ny + i] = num + SCM_BIGRAD; num = -1;}
      else {zds[j - ny + i] = num; num = 0;}
      t2 = SCM_BIGDN(t2);
    } while (++i < ny);
    num += zds[j - ny + i] - t2; /* borrow from high digit; don't update */
    while (num) {		/* "add back" required */
      i = 0; num = 0; qhat--;
      do {
	num += (long) zds[j - ny + i] + y[i];
	zds[j - ny + i] = SCM_BIGLO(num);
	num = SCM_BIGDN(num);
      } while (++i < ny);
      num--;
    }
    if (modes & 2) zds[j] = qhat;
  } while (--j >= ny);
  switch (modes) {
  case 3:			/* check that remainder==0 */
    for(j = ny;j && !zds[j-1];--j) ; if (j) return 0;
  case 2:			/* move quotient down in z */
    j = (nx==ny ? nx+2 : nx+1) - ny;
    for (i = 0;i < j;i++) zds[i] = zds[i+ny];
    ny = i;
    break;
  case 1:			/* subtract for scm_modulo */
    i = 0; num = 0; j = 0;
    do {num += y[i] - zds[i];
	j = j | zds[i];
	if (num < 0) {zds[i] = num + SCM_BIGRAD; num = -1;}
	else {zds[i] = num; num = 0;}
      } while (++i < ny);
    if (!j) return SCM_INUM0;
  case 0:			/* just normalize remainder */
    if (d) scm_divbigdig(zds, ny, d);
  }
 doadj:
  for(j = ny;j && !zds[j-1];--j) ;
  if (j * SCM_BITSPERDIG <= sizeof(SCM)*SCM_CHAR_BIT)
    if (SCM_INUMP(z = scm_big2inum(z, j)))
      return z;
  return scm_adjbig(z, j);
}
#endif





/*** NUMBERS -> STRINGS ***/
#ifdef SCM_FLOATS
int scm_dblprec;
static double fx[] = {
0.0, 5e-1, 5e-2, 5e-3, 5e-4, 5e-5,
			5e-6, 5e-7, 5e-8, 5e-9, 5e-10,
			5e-11,5e-12,5e-13,5e-14,5e-15,
			5e-16,5e-17,5e-18,5e-19,5e-20};



static size_t
idbl2str(double f, char *a)
{
  SCM_INTS_INDIFFERENT;

  int efmt, dpt, d, i, wp = scm_dblprec;
  size_t ch = 0;
  int exp = 0;

  if (f == 0.0) goto zero;	/*{a[0]='0'; a[1]='.'; a[2]='0'; return 3;}*/
  if (f < 0.0) {f = -f;a[ch++]='-';}
  else if (f > 0.0) ;
  else goto funny;
  if (IS_INF(f))
    {
      if (ch == 0) a[ch++]='+';
    funny: a[ch++]='#'; a[ch++]='.'; a[ch++]='#'; return ch;
    }
# ifdef DBL_MIN_10_EXP		/* Prevent unnormalized values, as from 
				   make-uniform-vector, from causing infinite loops. */
  while (f < 1.0) {f *= 10.0;  if (exp-- < DBL_MIN_10_EXP) goto funny;}
  while (f > 10.0) {f *= 0.10; if (exp++ > DBL_MAX_10_EXP) goto funny;}
# else
  while (f < 1.0) {f *= 10.0; exp--;}
  while (f > 10.0) {f /= 10.0; exp++;}
# endif
  if (f+fx[wp] >= 10.0) {f = 1.0; exp++;}
 zero:
# ifdef SCM_ENGNOT
  dpt = (exp+9999)%3;
  exp -= dpt++;
  efmt = 1;
# else
  efmt = (exp < -3) || (exp > wp+2);
  if (!efmt)
    if (exp < 0) {
      a[ch++] = '0';
      a[ch++] = '.';
      dpt = exp;
      while (++dpt)  a[ch++] = '0';
    } else
      dpt = exp+1;
  else
    dpt = 1;
# endif

  do {
    d = f;
    f -= d;
    a[ch++] = d+'0';
    if (f < fx[wp])  break;
    if (f+fx[wp] >= 1.0) {
      a[ch-1]++;
      break;
    }
    f *= 10.0;
    if (!(--dpt))  a[ch++] = '.';
  } while (wp--);

  if (dpt > 0)
    {
# ifndef SCM_ENGNOT
      if ((dpt > 4) && (exp > 6)) {
	d = (a[0]=='-'?2:1);
	for (i = ch++; i > d; i--)
	  a[i] = a[i-1];
	a[d] = '.';
	efmt = 1;
      } else
# endif
	{
	  while (--dpt)  a[ch++] = '0';
	  a[ch++] = '.';
	}
    }
  if (a[ch-1]=='.')  a[ch++]='0'; /* trailing zero */
  if (efmt && exp) {
    a[ch++] = 'e';
    if (exp < 0) {
      exp = -exp;
      a[ch++] = '-';
    }
    for (i = 10; i <= exp; i *= 10);
    for (i /= 10; i; i /= 10) {
      a[ch++] = exp/i + '0';
      exp %= i;
    }
  }
  return ch;
}

static size_t
iflo2str(SCM flt, char *str)
{
  SCM_INTS_INDIFFERENT;

  size_t i;
# ifdef SCM_SINGLES
  if (SCM_SINGP(flt))
    i = idbl2str(SCM_FLO(flt), str);
  else
# endif
    i = idbl2str(SCM_REAL(flt), str);
  if (SCM_CPLXP(flt))
    {
      if(0 <= SCM_IMAG(flt))	/* jeh */
	str[i++] = '+';		/* jeh */
      i += idbl2str(SCM_IMAG(flt), &str[i]);
      str[i++] = 'i';
    }
  return i;
}
#endif				/* SCM_FLOATS */

size_t
scm_iint2str(long num, int rad, char *p)
{
  SCM_INTS_INDIFFERENT;

  size_t j;
  int i = 1, d;
  long n = num;
  if (n < 0) {n = -n; i++;}
  for (n /= rad;n > 0;n /= rad) i++;
  j = i;
  n = num;
  if (n < 0) {n = -n; *p++ = '-'; i--;}
  while (i--) {
    d = n % rad;
    n /= rad;
    p[i] = d + ((d < 10) ? '0' : 'a' - 10);
  }
  return j;
}


#ifdef SCM_BIGDIG
static SCM
big2str(SCM b, unsigned int radix)
{
  SCM_INTS_ENABLED;

  SCM t = scm_copybig(b, 0);	/* sign of temp doesn't matter */
  SCM_BIGDIG *ds = SCM_BDIGITS(t);
  size_t i = SCM_NUMDIGS(t);
  size_t j = radix==16 ? (SCM_BITSPERDIG*i)/4+2
    : radix >= 10 ? (SCM_BITSPERDIG*i*241L)/800+2
      : (SCM_BITSPERDIG*i)+2;
  size_t k = 0;
  size_t radct = 0;
  size_t ch;			/* jeh */
  SCM_BIGDIG radpow = 1, radmod = 0;
  SCM ss = scm_makstr((long)j);
  char *s = SCM_STRING_CHARS(ss), c;
  while ((long) radpow * radix < SCM_BIGRAD) {
    radpow *= radix;
    radct++;
  }
  s[0] = scm_tc16_bigneg==SCM_TYP16(b) ? '-' : '+';
  while ((i || radmod) && j) {
    if (k == 0) {
      radmod = (SCM_BIGDIG)scm_divbigdig(ds, i, radpow);
      k = radct;
      if (!ds[i-1]) i--;
    }
    c = radmod % radix; radmod /= radix; k--;
    s[--j] = c < 10 ? c + '0' : c + 'a' - 10;
  }
  ch = s[0] == '-' ? 1 : 0;
  if (ch == j)
    return ss;
  else
    {
      SCM str;
      char * str_c;
      int x;
      str = scm_makstr (ch + SCM_LENGTH (ss) - j);
      str_c = SCM_STRING_CHARS (str);
      if (ch)
	str_c[0] = s[0];
      for(x = ch; j < SCM_LENGTH(ss); x++, j++)
	str_c[x] = s[j];
      return scm_return_first (str, ss);
    }
}
#endif


SCM_PROC(s_number_to_string, "number->string", 1, 1, 0, scm_number_to_string);
SCM
scm_number_to_string(SCM x, SCM radix)
{
  SCM_INTS_ENABLED;

  if (SCM_UNBNDP(radix))
    radix=SCM_MAKINUM(10L);
  else SCM_ASSERT(SCM_INUMP(radix), radix, scm_arg2, s_number_to_string);
#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
      char num_buf[SCM_FLOBUFLEN];
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	return big2str(x, (unsigned int)SCM_INUM(radix));
#  ifndef SCM_RECKLESS
      if (!(SCM_INEXP(x)))
      badx: scm_wta(x, scm_arg1, s_number_to_string);
#  endif
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_INEXP(x), x, scm_arg1, s_number_to_string);
# endif
      return scm_makfromstr(num_buf, iflo2str(x, num_buf));
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_number_to_string);
      return big2str(x, (unsigned int)SCM_INUM(radix));
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_number_to_string);
# endif
#endif
  {
    char num_buf[SCM_INTBUFLEN];
    return scm_makfromstr(num_buf,
			  scm_iint2str(SCM_INUM(x), (int)SCM_INUM(radix), num_buf));
  }
}


/* These print routines are stubbed here so that scm_repl.c doesn't need
   SCM_FLOATS or SCM_BIGDIGs conditionals */
int
scm_floprint(SCM sexp, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  int errn;

#ifdef SCM_FLOATS
  char num_buf[SCM_FLOBUFLEN];
  scm_port_write (&errn, port, num_buf, iflo2str(sexp, num_buf));
#else
  scm_ipruk("float", sexp, port);
#endif
  return !0;
}


int
scm_bigprint(SCM exp, SCM port, int writing)
{
  SCM_INTS_ENABLED;
  int errn;

#ifdef SCM_BIGDIG
  exp = big2str(exp, (unsigned int)10);
  SCM_DEFER_INTS;
  scm_port_write (&errn, port, SCM_RO_CHARS(exp), (size_t)SCM_LENGTH(exp));
  SCM_ALLOW_INTS;
#else
  scm_ipruk("bignum", exp, port);
#endif
  return !0;
}
/*** END nums->strs ***/

/*** STRINGS -> NUMBERS ***/
#ifdef SCM_BIGDIG
SCM
scm_istr2int(char *str, long len, long radix)
{
  SCM_INTS_ENABLED;

  size_t j;
  size_t k, blen = 1;
  size_t i = 0;
  int c;
  SCM res;
  SCM_BIGDIG *ds;
  unsigned long t2;

  if (0 >= len) return SCM_BOOL_F;	/* zero scm_length */
  if (16==radix) j = 1+(4*len*sizeof(char))/(SCM_BITSPERDIG);
  else if (10 <= radix)
    j = 1+(84*len*sizeof(char))/(SCM_BITSPERDIG*25);
  else j = 1+(len*sizeof(char))/(SCM_BITSPERDIG);
  switch (str[0]) {		/* leading sign */
  case '-':
  case '+': if (++i==len) return SCM_BOOL_F; /* bad if lone `+' or `-' */
  }
  res = scm_mkbig(j, '-'==str[0]);
  ds = SCM_BDIGITS(res);
  for (k = j;k--;) ds[k] = 0;
  do {
    switch (c = str[i++]) {
    case DIGITS:
      c = c - '0';
      goto accumulate;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      c = c-'A'+10;
      goto accumulate;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      c = c-'a'+10;
    accumulate:
      if (c >= radix) return SCM_BOOL_F; /* bad digit for radix */
      k = 0;
      t2 = c;
    moretodo:
      while(k < blen) {
	/*	printf("k = %d, blen = %d, t2 = %ld, ds[k] = %d\n", k, blen, t2, ds[k]);*/
	t2 += ds[k]*radix;
	ds[k++] = SCM_BIGLO(t2);
	t2 = SCM_BIGDN(t2);
      }
      SCM_ASSERT(blen <= j, (SCM)SCM_MAKINUM(blen), scm_ovflow, s_bignum);
      if (t2) {blen++; goto moretodo;}
      break;
    default:
      return SCM_BOOL_F;		/* not a digit */
    }
  } while (i < len);
  if (blen * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM))
    if (SCM_INUMP(res = scm_big2inum(res, blen)))
      return res;
  if (j==blen) return res;
  return scm_adjbig(res, blen);
}
#else



SCM
scm_istr2int(char *str, long len, long radix)
{
  long n = 0, ln;
  int c;
  int i = 0;
  int lead_neg = 0;
  if (0 >= len) return SCM_BOOL_F;	/* zero scm_length */
  switch (*str) {		/* leading sign */
  case '-': lead_neg = 1;
  case '+': if (++i==len) return SCM_BOOL_F; /* bad if lone `+' or `-' */
  }

  do {
    switch (c = str[i++]) {
    case DIGITS:
      c = c - '0';
      goto accumulate;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      c = c-'A'+10;
      goto accumulate;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      c = c-'a'+10;
    accumulate:
      if (c >= radix) return SCM_BOOL_F; /* bad digit for radix */
      ln = n;
      n = n * radix - c;
      /* Negation is a workaround for HP700 cc bug */
      if (n > ln || (-n > -SCM_MOST_NEGATIVE_FIXNUM)) goto ovfl;
      break;
    default:
      return SCM_BOOL_F;		/* not a digit */
    }
  } while (i < len);
  if (!lead_neg) if ((n = -n) > SCM_MOST_POSITIVE_FIXNUM) goto ovfl;
  return SCM_MAKINUM(n);
 ovfl:				/* overflow scheme integer */
  return SCM_BOOL_F;
}
#endif

#ifdef SCM_FLOATS
SCM
scm_istr2flo(char *str, long len, long radix)
{
  SCM_INTS_DISABLED;

  int c, i = 0;
  double lead_sgn;
  double res = 0.0, tmp = 0.0;
  int flg = 0;
  int point = 0;
  SCM second;

  if (i >= len) return SCM_BOOL_F;	/* zero scm_length */

  switch (*str) {		/* leading sign */
  case '-': lead_sgn = -1.0; i++; break;
  case '+': lead_sgn = 1.0; i++; break;
  default : lead_sgn = 0.0;
  }
  if (i==len) return SCM_BOOL_F;	/* bad if lone `+' or `-' */

  if (str[i]=='i' || str[i]=='I') { /* handle `+i' and `-i'   */
    if (lead_sgn==0.0) return SCM_BOOL_F; /* must have leading sign */
    if (++i < len) return SCM_BOOL_F; /* `i' not last character */
    return scm_makdbl(0.0, lead_sgn);
  }
  do {				/* check initial digits */
    switch (c = str[i]) {
    case DIGITS:
      c = c - '0';
      goto accum1;
    case 'D': case 'E': case 'F':
      if (radix==10) goto out1; /* must be exponent */
    case 'A': case 'B': case 'C':
      c = c-'A'+10;
      goto accum1;
    case 'd': case 'e': case 'f':
      if (radix==10) goto out1;
    case 'a': case 'b': case 'c':
      c = c-'a'+10;
    accum1:
      if (c >= radix) return SCM_BOOL_F; /* bad digit for radix */
      res = res * radix + c;
      flg = 1;			/* res is valid */
      break;
    default:
      goto out1;
    }
  } while (++i < len);
 out1:

  /* if true, then we did see a digit above, and res is valid */
  if (i==len) goto done;

  /* By here, must have seen a digit,
     or must have next char be a `.' with radix==10 */
  if (!flg)
    if (!(str[i]=='.' && radix==10))
      return SCM_BOOL_F;

  while (str[i]=='#') {		/* optional sharps */
    res *= radix;
    if (++i==len) goto done;
  }

  if (str[i]=='/') {
    while (++i < len) {
      switch (c = str[i]) {
      case DIGITS:
	c = c - '0';
	goto accum2;
      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	c = c-'A'+10;
	goto accum2;
      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	c = c-'a'+10;
      accum2:
	if (c >= radix) return SCM_BOOL_F;
	tmp = tmp * radix + c;
	break;
      default:
	goto out2;
      }
    }
  out2:
    if (tmp==0.0) return SCM_BOOL_F; /* `slash zero' not allowed */
    if (i < len)
      while (str[i]=='#') {	/* optional sharps */
	tmp *= radix;
	if (++i==len) break;
      }
    res /= tmp;
    goto done;
  }

  if (str[i]=='.') {		/* decimal point notation */
    if (radix != 10) return SCM_BOOL_F; /* must be radix 10 */
    while (++i < len) {
      switch (c = str[i]) {
      case DIGITS:
	point--;
	res = res*10.0 + c-'0';
	flg = 1;
	break;
      default:
	goto out3;
      }
    }
  out3:
    if (!flg) return SCM_BOOL_F;	/* no digits before or after decimal point */
    if (i==len) goto adjust;
    while (str[i]=='#') {	/* ignore remaining sharps */
      if (++i==len) goto adjust;
    }
  }

  switch (str[i]) {		/* exponent */
  case 'd': case 'D':
  case 'e': case 'E':
  case 'f': case 'F':
  case 'l': case 'L':
  case 's': case 'S': {
    int expsgn = 1, expon = 0;
    if (radix != 10) return SCM_BOOL_F; /* only in radix 10 */
    if (++i==len) return SCM_BOOL_F; /* bad exponent */
    switch (str[i]) {
    case '-':  expsgn=(-1);
    case '+':  if (++i==len) return SCM_BOOL_F; /* bad exponent */
    }
    if (str[i] < '0' || str[i] > '9') return SCM_BOOL_F; /* bad exponent */
    do {
      switch (c = str[i]) {
      case DIGITS:
	expon = expon*10 + c-'0';
	if (expon > MAXEXP)  return SCM_BOOL_F; /* exponent too large */
	break;
      default:
	goto out4;
      }
    } while (++i < len);
  out4:
    point += expsgn*expon;
  }
  }

 adjust:
  if (point >= 0)
    while (point--)  res *= 10.0;
  else
# ifdef _UNICOS
    while (point++)  res *= 0.1; 
# else
  while (point++)  res /= 10.0;
# endif

 done:
  /* at this point, we have a legitimate floating point result */
  if (lead_sgn==-1.0)  res = -res;
  if (i==len) return scm_makdbl(res, 0.0);

  if (str[i]=='i' || str[i]=='I') { /* pure imaginary number  */
    if (lead_sgn==0.0) return SCM_BOOL_F; /* must have leading sign */
    if (++i < len) return SCM_BOOL_F; /* `i' not last character */
    return scm_makdbl(0.0, res);
  }

  switch (str[i++]) {
  case '-':  lead_sgn = -1.0; break;
  case '+':  lead_sgn = 1.0;  break;
  case '@': {			/* polar input for complex number */
    /* get a `real' for scm_angle */
    second = scm_istr2flo(&str[i], (long)(len-i), radix);
    if (!(SCM_INEXP(second))) return SCM_BOOL_F; /* not `real' */
    if (SCM_CPLXP(second))    return SCM_BOOL_F; /* not `real' */
    tmp = SCM_REALPART(second);
    return scm_makdbl(res*cos(tmp), res*sin(tmp));
  }
  default: return SCM_BOOL_F;
  }

  /* at this point, last char must be `i' */
  if (str[len-1] != 'i' && str[len-1] != 'I') return SCM_BOOL_F;
  /* handles `x+i' and `x-i' */
  if (i==(len-1))  return scm_makdbl(res, lead_sgn);
  /* get a `ureal' for complex part */
  second = scm_istr2flo(&str[i], (long)((len-i)-1), radix);
  if (!(SCM_INEXP(second))) return SCM_BOOL_F; /* not `ureal' */
  if (SCM_CPLXP(second))    return SCM_BOOL_F; /* not `ureal' */
  tmp = SCM_REALPART(second);
  if (tmp < 0.0)	return SCM_BOOL_F; /* not `ureal' */
  return scm_makdbl(res, (lead_sgn*tmp));
}
#endif				/* SCM_FLOATS */


SCM
scm_istring2number(char *str, long len, long radix)
{
  SCM_INTS_ENABLED;

  int i = 0;
  char ex = 0;
  char ex_p = 0, rx_p = 0;	/* Only allow 1 exactness and 1 radix prefix */
  SCM res;
  if (len==1)
    if (*str=='+' || *str=='-') /* Catches lone `+' and `-' for speed */
      return SCM_BOOL_F;

  while ((len-i) >= 2  &&  str[i]=='#' && ++i)
    switch (str[i++]) {
    case 'b': case 'B':  if (rx_p++) return SCM_BOOL_F; radix = 2;  break;
    case 'o': case 'O':  if (rx_p++) return SCM_BOOL_F; radix = 8;  break;
    case 'd': case 'D':  if (rx_p++) return SCM_BOOL_F; radix = 10; break;
    case 'x': case 'X':  if (rx_p++) return SCM_BOOL_F; radix = 16; break;
    case 'i': case 'I':  if (ex_p++) return SCM_BOOL_F; ex = 2;     break;
    case 'e': case 'E':  if (ex_p++) return SCM_BOOL_F; ex = 1;     break;
    default:  return SCM_BOOL_F;
    }

  switch (ex) {
  case 1:
    return scm_istr2int(&str[i], len-i, radix);
  case 0:
    res = scm_istr2int(&str[i], len-i, radix);
    if (SCM_BOOL_F != res)
      return res;
#ifdef SCM_FLOATS
  case 2: return scm_istr2flo(&str[i], len-i, radix);
#endif
  }
  return SCM_BOOL_F;
}


SCM_PROC(s_string_to_number, "string->number", 1, 1, 0, scm_string_to_number);
SCM
scm_string_to_number(SCM str, SCM radix)
{
  SCM_INTS_ENABLED;

  SCM answer;
  if (SCM_UNBNDP(radix))
    radix=SCM_MAKINUM(10L);
  else SCM_ASSERT(SCM_INUMP(radix), radix, scm_arg2, s_string_to_number);
  SCM_ASSERT(scm_is_ro_string(str), str, scm_arg1, s_string_to_number);
  answer = scm_istring2number(SCM_RO_CHARS(str), SCM_RO_LENGTH(str), SCM_INUM(radix));
  return scm_return_first (answer, str);
}
/*** END strs->nums ***/

#ifdef SCM_FLOATS
SCM
scm_makdbl (double x, double y)
{
  SCM_INTS_NESTED;

  SCM z;
  if ((y==0.0) && (x==0.0)) return scm_flo0;

  SCM_NEWCELL(z);


  SCM_REDEFER_INTS;
  if (y==0.0)
    {
# ifdef SCM_SINGLES
      float fx = x;
#  ifndef SCM_SINGLESONLY
      if ((-FLTMAX < x) && (x < FLTMAX) && (fx==x))
#  endif
	{
	  SCM_CAR(z) = scm_tc16_flo;
	  SCM_FLO(z) = x;
	  goto done;
	}
# endif/* def SCM_SINGLES */
      SCM_CDR(z) = (SCM)scm_must_malloc(1L*sizeof(double));
      SCM_CAR(z) = scm_tc_dblr;
    }
  else
    {
      SCM_CDR(z) = (SCM)scm_must_malloc(2L*sizeof(double));
      SCM_CAR(z) = scm_tc_dblc;
      SCM_IMAG(z) = y;
    }
  SCM_REAL(z) = x;

 done:
  SCM_REALLOW_INTS;
  return z;
}
#endif


SCM
scm_bigequal(SCM x, SCM y)
{
  SCM_INTS_INDIFFERENT;

#ifdef SCM_BIGDIG
  if (0==scm_bigcomp(x, y)) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}


SCM
scm_floequal(SCM x, SCM y)
{
  SCM_INTS_INDIFFERENT;

#ifdef SCM_FLOATS
  if (SCM_REALPART(x) != SCM_REALPART(y)) return SCM_BOOL_F;
  if (!(SCM_CPLXP(x) && (SCM_IMAG(x) != SCM_IMAG(y)))) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}




SCM_PROC(s_number_p, "number?", 1, 0, 0, scm_number_p);
SCM_PROC(s_complex_p, "complex?", 1, 0, 0, scm_number_p);
SCM
scm_number_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  if (SCM_INUMP(x))
    return SCM_BOOL_T;
#ifdef SCM_FLOATS
  if (!SCM_IS_IMMEDIATE(x) && SCM_NUMP(x)) return SCM_BOOL_T;
#else
# ifdef SCM_BIGDIG
  if (!SCM_IS_IMMEDIATE(x) && SCM_NUMP(x)) return SCM_BOOL_T;
# endif
#endif
  return SCM_BOOL_F;
}



#ifdef SCM_FLOATS
SCM_PROC(s_real_p, "real?", 1, 0, 0, scm_real_p);
SCM_PROC(s_rational_p, "rational?", 1, 0, 0, scm_real_p);
SCM
scm_real_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  if (SCM_INUMP(x))
    return SCM_BOOL_T;
  if (SCM_IS_IMMEDIATE(x))
    return SCM_BOOL_F;
  if (SCM_REALP(x))
    return SCM_BOOL_T;
# ifdef SCM_BIGDIG
  if (SCM_BIGP(x))
    return SCM_BOOL_T;
# endif
  return SCM_BOOL_F;
}



SCM_PROC(s_int_p, "int?", 1, 0, 0, scm_int_p);
SCM
scm_int_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  double r;
  if (SCM_INUMP(x))
    return SCM_BOOL_T;
  if (SCM_IS_IMMEDIATE(x))
    return SCM_BOOL_F;
# ifdef SCM_BIGDIG
  if (SCM_BIGP(x))
    return SCM_BOOL_T;
# endif
  if (!SCM_INEXP(x)) return SCM_BOOL_F;
  if (SCM_CPLXP(x))
    return SCM_BOOL_F;
  r = SCM_REALPART(x);
  if (r==floor(r)) return SCM_BOOL_T;
  return SCM_BOOL_F;
}



#endif				/* SCM_FLOATS */

SCM_PROC(s_inexact_p, "inexact?", 1, 0, 0, scm_inexact_p);
SCM
scm_inexact_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

#ifdef SCM_FLOATS
  if (!SCM_IS_IMMEDIATE(x) && SCM_INEXP(x)) return SCM_BOOL_T;
#endif
  return SCM_BOOL_F;
}




SCM_PROC1 (s_eq_p, "=?", scm_tc7_rpsubr, scm_num_eq_p);
SCM
scm_num_eq_p (SCM x, SCM y)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  SCM t;
  if (SCM_NINUMP(x))
    {
# ifdef SCM_BIGDIG
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(x)))
      badx: scm_wta(x, scm_arg1, s_eq_p);
#  endif
      if (SCM_BIGP(x))
	{
	  if (SCM_INUMP(y))
	    return SCM_BOOL_F;
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    return (0==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
	  if (!SCM_INEXP(y))
	    goto bady;
	bigreal:
	  return (SCM_REALP(y) && (scm_big2dbl(x)==SCM_REALPART(y))) ? SCM_BOOL_T : SCM_BOOL_F;
	}
      if (!SCM_INEXP(x))
	goto badx;
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_INEXP(x), x, scm_arg1, s_eq_p);
# endif
      if (SCM_INUMP(y))
	{
	  t = x;
	  x = y;
	  y = t;
	  goto realint;
	}
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{
	  t = x;
	  x = y;
	  y = t;
	  goto bigreal;
	}
      if (!SCM_INEXP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_INEXP(y))
	goto bady;
# endif
      if (SCM_REALPART(x) != SCM_REALPART(y)) return SCM_BOOL_F;
      if (SCM_CPLXP(x))
	return (SCM_CPLXP(y) && (SCM_IMAG(x)==SCM_IMAG(y))) ? SCM_BOOL_T : SCM_BOOL_F;
      return SCM_CPLXP(y) ? SCM_BOOL_F : SCM_BOOL_T;
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return SCM_BOOL_F;
#  ifndef SCM_RECKLESS
      if (!(SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_eq_p);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_eq_p);
#  endif
# endif
    realint:
      return (SCM_REALP(y) && (((double)SCM_INUM(x))==SCM_REALPART(y))) ? SCM_BOOL_T : SCM_BOOL_F;
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_eq_p);
      if (SCM_INUMP(y))
	return SCM_BOOL_F;
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      return (0==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_eq_p);
#  endif
      return SCM_BOOL_F;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_eq_p);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_eq_p);
# endif
#endif
  return ((long)x==(long)y) ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC1 (s_lt_p, "<?", scm_tc7_rpsubr, scm_lt_p);
SCM
scm_lt_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
# ifdef SCM_BIGDIG
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(x)))
      badx: scm_wta(x, scm_arg1, s_lt_p);
#  endif
      if (SCM_BIGP(x))
	{
	  if (SCM_INUMP(y))
	    return SCM_BIGSIGN(x) ? SCM_BOOL_T : SCM_BOOL_F;
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    return (1==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
	  if (!SCM_REALP(y))
	    goto bady;
	  return (scm_big2dbl(x) < SCM_REALPART(y)) ? SCM_BOOL_T : SCM_BOOL_F;
	}
      if (!SCM_REALP(x))
	goto badx;
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_REALP(x), x, scm_arg1, s_lt_p);
# endif
      if (SCM_INUMP(y))
	return (SCM_REALPART(x) < ((double)SCM_INUM(y))) ? SCM_BOOL_T : SCM_BOOL_F;
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return (SCM_REALPART(x) < scm_big2dbl(y)) ? SCM_BOOL_T : SCM_BOOL_F;
      if (!SCM_REALP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_REALP(y))
	goto bady;
# endif
      return (SCM_REALPART(x) < SCM_REALPART(y)) ? SCM_BOOL_T : SCM_BOOL_F;
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return SCM_BIGSIGN(y) ? SCM_BOOL_F : SCM_BOOL_T;
#  ifndef SCM_RECKLESS
      if (!(SCM_REALP(y)))
      bady: scm_wta(y, scm_arg2, s_lt_p);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_REALP(y)))
      bady: scm_wta(y, scm_arg2, s_lt_p);
#  endif
# endif
      return (((double)SCM_INUM(x)) < SCM_REALPART(y)) ? SCM_BOOL_T : SCM_BOOL_F;
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_lt_p);
      if (SCM_INUMP(y))
	return SCM_BIGSIGN(x) ? SCM_BOOL_T : SCM_BOOL_F;
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      return (1==scm_bigcomp(x, y)) ? SCM_BOOL_T : SCM_BOOL_F;
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_lt_p);
#  endif
      return SCM_BIGSIGN(y) ? SCM_BOOL_F : SCM_BOOL_T;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_lt_p);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_lt_p);
# endif
#endif
  return ((long)x < (long)y) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC1 (s_gt_p, ">?", scm_tc7_rpsubr, scm_gt_p);
SCM
scm_gt_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  return scm_lt_p(y, x);
}



SCM_PROC1 (s_le_p, "<=?", scm_tc7_rpsubr, scm_le_p);
SCM
scm_le_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  return scm_not (scm_lt_p (y, x));
}



SCM_PROC1 (s_ge_p, ">=?", scm_tc7_rpsubr, scm_ge_p);
SCM
scm_ge_p(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  return scm_not (scm_lt_p (x, y));
}



SCM_PROC(s_zero_p, "zero?", 1, 0, 0, scm_zero_p);
SCM
scm_zero_p(SCM z)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  if (SCM_NINUMP(z))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(z))
	goto badz;
      if (SCM_BIGP(z))
	return SCM_BOOL_F;
#  ifndef SCM_RECKLESS
      if (!(SCM_INEXP(z)))
      badz: scm_wta(z, scm_arg1, s_zero_p);
#  endif
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_INEXP(z), z, scm_arg1, s_zero_p);
# endif
      return (z==scm_flo0) ? SCM_BOOL_T : SCM_BOOL_F;
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(z))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_BIGP(z), z, scm_arg1, s_zero_p);
      return SCM_BOOL_F;
    }
# else
  SCM_ASSERT(SCM_INUMP(z), z, scm_arg1, s_zero_p);
# endif
#endif
  return (z==SCM_INUM0) ? SCM_BOOL_T: SCM_BOOL_F;
}



SCM_PROC(s_positive_p, "positive?", 1, 0, 0, scm_positive_p);
SCM
scm_positive_p(SCM x)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	return SCM_TYP16(x)==scm_tc16_bigpos ? SCM_BOOL_T : SCM_BOOL_F;
#  ifndef SCM_RECKLESS
      if (!(SCM_REALP(x)))
      badx: scm_wta(x, scm_arg1, s_positive_p);
#  endif
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_REALP(x), x, scm_arg1, s_positive_p);
# endif
      return (SCM_REALPART(x) > 0.0) ? SCM_BOOL_T : SCM_BOOL_F;
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_positive_p);
      return SCM_TYP16(x)==scm_tc16_bigpos ? SCM_BOOL_T : SCM_BOOL_F;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_positive_p);
# endif
#endif
  return (x > SCM_INUM0) ? SCM_BOOL_T : SCM_BOOL_F;
}



SCM_PROC(s_negative_p, "negative?", 1, 0, 0, scm_negative_p);
SCM
scm_negative_p(SCM x)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	return SCM_TYP16(x)==scm_tc16_bigpos ? SCM_BOOL_F : SCM_BOOL_T;
#  ifndef SCM_RECKLESS
      if (!(SCM_REALP(x)))
      badx: scm_wta(x, scm_arg1, s_negative_p);
#  endif
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_REALP(x), x, scm_arg1, s_negative_p);
# endif
      return (SCM_REALPART(x) < 0.0) ? SCM_BOOL_T : SCM_BOOL_F;
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_negative_p);
      return (SCM_TYP16(x)==scm_tc16_bigneg) ? SCM_BOOL_T : SCM_BOOL_F;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_negative_p);
# endif
#endif
  return (x < SCM_INUM0) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_PROC1 (s_max, "max", scm_tc7_asubr, scm_max);
SCM
scm_max(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  double z;
#endif
  if (SCM_UNBNDP(y))
    {
#ifndef SCM_RECKLESS
      if (!(SCM_NUMBERP(x)))
      badx: scm_wta(x, scm_arg1, s_max);
#endif
      return x;
    }
#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	{
	  if (SCM_INUMP(y))
	    return SCM_BIGSIGN(x) ? y : x;
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    return (1==scm_bigcomp(x, y)) ? y : x;
	  if (!SCM_REALP(y))
	    goto bady;
	  z = scm_big2dbl(x);
	  return (z < SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
	}
      if (!SCM_REALP(x))
	goto badx;
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_REALP(x), x, scm_arg1, s_max);
# endif
      if (SCM_INUMP(y))
	return (SCM_REALPART(x) < (z = SCM_INUM(y))) ? scm_makdbl(z, 0.0) : x;
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return (SCM_REALPART(x) < (z = scm_big2dbl(y))) ? scm_makdbl(z, 0.0) : x;
      if (!SCM_REALP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_REALP(y))
	goto bady;
# endif
      return (SCM_REALPART(x) < SCM_REALPART(y)) ? y : x;
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return SCM_BIGSIGN(y) ? x : y;
#  ifndef SCM_RECKLESS
      if (!(SCM_REALP(y)))
      bady: scm_wta(y, scm_arg2, s_max);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_REALP(y)))
      bady: scm_wta(y, scm_arg2, s_max);
#  endif
# endif
      return ((z = SCM_INUM(x)) < SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_max);
      if (SCM_INUMP(y))
	return SCM_BIGSIGN(x) ? y : x;
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      return (1==scm_bigcomp(x, y)) ? y : x;
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_max);
#  endif
      return SCM_BIGSIGN(y) ? x : y;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_max);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_max);
# endif
#endif
  return ((long)x < (long)y) ? y : x;
}




SCM_PROC1 (s_min, "min", scm_tc7_asubr, scm_min);
SCM
scm_min(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  double z;
#endif
  if (SCM_UNBNDP(y))
    {
#ifndef SCM_RECKLESS
      if (!(SCM_NUMBERP(x)))
      badx:scm_wta(x, scm_arg1, s_min);
#endif
      return x;
    }
#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	{
	  if (SCM_INUMP(y))
	    return SCM_BIGSIGN(x) ? x : y;
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    return (-1==scm_bigcomp(x, y)) ? y : x;
	  if (!SCM_REALP(y))
	    goto bady;
	  z = scm_big2dbl(x);
	  return (z > SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
	}
      if (!SCM_REALP(x))
	goto badx;
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_REALP(x), x, scm_arg1, s_min);
# endif
      if (SCM_INUMP(y))
	return (SCM_REALPART(x) > (z = SCM_INUM(y))) ? scm_makdbl(z, 0.0) : x;
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return (SCM_REALPART(x) > (z = scm_big2dbl(y))) ? scm_makdbl(z, 0.0) : x;
      if (!SCM_REALP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_REALP(y))
	goto bady;
# endif
      return (SCM_REALPART(x) > SCM_REALPART(y)) ? y : x;
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return SCM_BIGSIGN(y) ? y : x;
#  ifndef SCM_RECKLESS
      if (!(SCM_REALP(y)))
      bady: scm_wta(y, scm_arg2, s_min);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_REALP(y)))
      bady: scm_wta(y, scm_arg2, s_min);
#  endif
# endif
      return ((z = SCM_INUM(x)) > SCM_REALPART(y)) ? y : scm_makdbl(z, 0.0);
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_min);
      if (SCM_INUMP(y))
	return SCM_BIGSIGN(x) ? x : y;
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      return (-1==scm_bigcomp(x, y)) ? y : x;
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_min);
#  endif
      return SCM_BIGSIGN(y) ? y : x;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_min);
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_min);
# endif
#endif
  return ((long)x > (long)y) ? y : x;
}




SCM_PROC1 (s_sum, "+", scm_tc7_asubr, scm_sum);
SCM
scm_sum(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  if (SCM_UNBNDP(y))
    {
      if (SCM_UNBNDP(x))
	return SCM_INUM0;
#ifndef SCM_RECKLESS
      if (!(SCM_NUMBERP(x)))
      badx: scm_wta(x, scm_arg1, s_sum);
#endif
      return x;
    }
#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
      SCM t;
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	{
	  if (SCM_INUMP(y))
	    {
	      t = x;
	      x = y;
	      y = t;
	      goto intbig;
	    }
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    {
	      if (SCM_NUMDIGS(x) > SCM_NUMDIGS(y)) {t = x; x = y; y = t;}
	      return scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0);
	    }
	  if (!SCM_INEXP(y))
	    goto bady;
	bigreal: return scm_makdbl(scm_big2dbl(x)+SCM_REALPART(y), SCM_CPLXP(y)?SCM_IMAG(y):0.0);
	}
      if (!SCM_INEXP(x))
	goto badx;
# else
      if (!!SCM_IS_IMMEDIATE(x) || !SCM_INEXP(x))
	goto badx;
# endif
      if (SCM_INUMP(y))
	{
	  t = x;
	  x = y;
	  y = t;
	  goto intreal;
	}
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{
	  t = x;
	  x = y;
	  y = t;
	  goto bigreal;
	}
#  ifndef SCM_RECKLESS
      else if (!(SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_sum);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_sum);
#  endif
# endif
      { double i = 0.0;
	if (SCM_CPLXP(x))
	  i = SCM_IMAG(x);
	if (SCM_CPLXP(y))
	  i += SCM_IMAG(y);
	return scm_makdbl(SCM_REALPART(x)+SCM_REALPART(y), i); }
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{
#  ifndef SCM_DIGSTOOBIG
	  long z;
	intbig: 
	  z = scm_pseudolong(SCM_INUM(x));
	  return scm_addbig((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  else
	  SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	intbig: 
	  scm_longdigs(SCM_INUM(x), zdigs);
	  return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  endif
	}
      if (!SCM_INEXP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_INEXP(y))
	goto bady;
# endif
    intreal: return scm_makdbl(SCM_INUM(x)+SCM_REALPART(y), SCM_CPLXP(y)?SCM_IMAG(y):0.0);
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM t;
      if (!!SCM_IS_IMMEDIATE(x) || !SCM_BIGP(x))
	goto badx;
      if (SCM_INUMP(y))
	{
	  t = x;
	  x = y;
	  y = t;
	  goto intbig;
	}
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      if (SCM_NUMDIGS(x) > SCM_NUMDIGS(y)) {t = x; x = y; y = t;}
      return scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0);
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_sum);
#  endif
    intbig:
      {
#  ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong(SCM_INUM(x));
	return scm_addbig(&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  else
	SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	scm_longdigs(SCM_INUM(x), zdigs);
	return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  endif
      }
    }
# else
  if (!SCM_INUMP(x))
    goto badx;
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_sum);
# endif
#endif
  x = SCM_INUM(x)+SCM_INUM(y);
  if (SCM_FIXABLE(x))
    return SCM_MAKINUM(x);
#ifdef SCM_BIGDIG
  return scm_long2big(x);
#else
# ifdef SCM_FLOATS
  return scm_makdbl((double)x, 0.0);
# else
  scm_wta(y, scm_ovflow, s_sum);
  return SCM_UNSPECIFIED;
# endif
#endif
}




SCM_PROC1 (s_difference, "-", scm_tc7_asubr, scm_difference);
SCM
scm_difference(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
# ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(x)))
      badx: scm_wta(x, scm_arg1, s_difference);
# endif
      if (SCM_UNBNDP(y))
	{
# ifdef SCM_BIGDIG
	  if (SCM_BIGP(x))
	    {
	      x = scm_copybig(x, !SCM_BIGSIGN(x));
	      return SCM_NUMDIGS(x) * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM) ?
		scm_big2inum(x, SCM_NUMDIGS(x)) : x;
	    }
# endif
	  if (!SCM_INEXP(x))
	    goto badx;
	  return scm_makdbl(-SCM_REALPART(x), SCM_CPLXP(x)?-SCM_IMAG(x):0.0);
	}
      if (SCM_INUMP(y))
	return scm_sum(x, SCM_MAKINUM(-SCM_INUM(y)));
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(x))
	{
	  if (SCM_BIGP(y))
	    return ((SCM_NUMDIGS(x) < SCM_NUMDIGS(y))
		    ? scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0x0100)
		    : scm_addbig(SCM_BDIGITS(y), SCM_NUMDIGS(y), SCM_BIGSIGN(y) ^ 0x0100, x, 0));
	  if (!SCM_INEXP(y))
	    goto bady;
	  return scm_makdbl(scm_big2dbl(x)-SCM_REALPART(y), SCM_CPLXP(y)?-SCM_IMAG(y):0.0);
	}
      if (!SCM_INEXP(x))
	goto badx;
      if (SCM_BIGP(y))
	return scm_makdbl(SCM_REALPART(x)-scm_big2dbl(y), SCM_CPLXP(x)?SCM_IMAG(x):0.0);
      if (!SCM_INEXP(y))
	goto bady;
# else
      if (!SCM_INEXP(x))
	goto badx;
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_INEXP(y))
	goto bady;
# endif
      if (SCM_CPLXP(x))
	{
	  if (SCM_CPLXP(y))
	    return scm_makdbl(SCM_REAL(x)-SCM_REAL(y), SCM_IMAG(x)-SCM_IMAG(y));
	  else
	    return scm_makdbl(SCM_REAL(x)-SCM_REALPART(y), SCM_IMAG(x));
	}
      return scm_makdbl(SCM_REALPART(x)-SCM_REALPART(y), SCM_CPLXP(y)?-SCM_IMAG(y):0.0);
    }
  if (SCM_UNBNDP(y))
    {
      x = -SCM_INUM(x);
      goto checkx;
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{
#  ifndef SCM_DIGSTOOBIG
	  long z = scm_pseudolong(SCM_INUM(x));
	  return scm_addbig((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  else
	  SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	  scm_longdigs(SCM_INUM(x), zdigs);
	  return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  endif
	}
#  ifndef SCM_RECKLESS
      if (!(SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_difference);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_difference);
#  endif
# endif
      return scm_makdbl(SCM_INUM(x)-SCM_REALPART(y), SCM_CPLXP(y)?-SCM_IMAG(y):0.0);
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_difference);
      if (SCM_UNBNDP(y))
	{
	  x = scm_copybig(x, !SCM_BIGSIGN(x));
	  return SCM_NUMDIGS(x) * SCM_BITSPERDIG/SCM_CHAR_BIT <= sizeof(SCM) ?
	    scm_big2inum(x, SCM_NUMDIGS(x)) : x;
	}
      if (SCM_INUMP(y))
	{
#  ifndef SCM_DIGSTOOBIG
	  long z = scm_pseudolong(SCM_INUM(y));
	  return scm_addbig(&z, SCM_DIGSPERLONG, (y < 0) ? 0 : 0x0100, x, 0);
#  else
	  SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	  scm_longdigs(SCM_INUM(x), zdigs);
	  return scm_addbig(zdigs, SCM_DIGSPERLONG, (y < 0) ? 0 : 0x0100, x, 0);
#  endif
	}
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      return (SCM_NUMDIGS(x) < SCM_NUMDIGS(y)) ?
	scm_addbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BIGSIGN(x), y, 0x0100) :
      scm_addbig(SCM_BDIGITS(y), SCM_NUMDIGS(y), SCM_BIGSIGN(y) ^ 0x0100, x, 0);
    }
  if (SCM_UNBNDP(y))
    {
      x = -SCM_INUM(x);
      goto checkx;
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_difference);
#  endif
      {
#  ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong(SCM_INUM(x));
	return scm_addbig(&z, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  else
	SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	scm_longdigs(SCM_INUM(x), zdigs);
	return scm_addbig(zdigs, SCM_DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  endif
      }
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_difference);
  if (SCM_UNBNDP(y))
    {
      x = -SCM_INUM(x);
      goto checkx;
    }
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_difference);
# endif
#endif
  x = SCM_INUM(x)-SCM_INUM(y);
 checkx:
  if (SCM_FIXABLE(x))
    return SCM_MAKINUM(x);
#ifdef SCM_BIGDIG
  return scm_long2big(x);
#else
# ifdef SCM_FLOATS
  return scm_makdbl((double)x, 0.0);
# else
  scm_wta(y, scm_ovflow, s_difference);
  return SCM_UNSPECIFIED;
# endif
#endif
}




SCM_PROC1 (s_product, "*", scm_tc7_asubr, scm_product);
SCM
scm_product(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

  if (SCM_UNBNDP(y))
    {
      if (SCM_UNBNDP(x))
	return SCM_MAKINUM(1L);
#ifndef SCM_RECKLESS
      if (!(SCM_NUMBERP(x)))
      badx: scm_wta(x, scm_arg1, s_product);
#endif
      return x;
    }
#ifdef SCM_FLOATS
  if (SCM_NINUMP(x))
    {
      SCM t;
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(x))
	goto badx;
      if (SCM_BIGP(x))
	{
	  if (SCM_INUMP(y))
	    {
	      t = x;
	      x = y;
	      y = t;
	      goto intbig;
	    }
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    return scm_mulbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			      SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y));
	  if (!SCM_INEXP(y))
	    goto bady;
	bigreal: {
		   double bg = scm_big2dbl(x);
		   return scm_makdbl(bg*SCM_REALPART(y), SCM_CPLXP(y)?bg*SCM_IMAG(y):0.0); }
	}
      if (!SCM_INEXP(x))
	goto badx;
# else
      if (!!SCM_IS_IMMEDIATE(x) || !SCM_INEXP(x))
	goto badx;
# endif
      if (SCM_INUMP(y))
	{t = x; x = y; y = t; goto intreal;}
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{t = x; x = y; y = t; goto bigreal;}
#  ifndef SCM_RECKLESS
      else if (!(SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_product);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_product);
#  endif
# endif
      if (SCM_CPLXP(x))
	{
	  if (SCM_CPLXP(y))
	    return scm_makdbl(SCM_REAL(x)*SCM_REAL(y)-SCM_IMAG(x)*SCM_IMAG(y),
			      SCM_REAL(x)*SCM_IMAG(y)+SCM_IMAG(x)*SCM_REAL(y));
	  else
	    return scm_makdbl(SCM_REAL(x)*SCM_REALPART(y), SCM_IMAG(x)*SCM_REALPART(y));
	}
      return scm_makdbl(SCM_REALPART(x)*SCM_REALPART(y),
			SCM_CPLXP(y)?SCM_REALPART(x)*SCM_IMAG(y):0.0);
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{
	intbig: if (SCM_INUM0==x) return x; if (SCM_MAKINUM(1L)==x) return y;
	  {
#  ifndef SCM_DIGSTOOBIG
	    long z = scm_pseudolong(SCM_INUM(x));
	    return scm_mulbig((SCM_BIGDIG *)&z, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			      SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  else
	    SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	    scm_longdigs(SCM_INUM(x), zdigs);
	    return scm_mulbig(zdigs, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			      SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  endif
	  }
	}
      if (!SCM_INEXP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_INEXP(y))
	goto bady;
# endif
    intreal: return scm_makdbl(SCM_INUM(x)*SCM_REALPART(y), SCM_CPLXP(y)?SCM_INUM(x)*SCM_IMAG(y):0.0);
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      if (!!SCM_IS_IMMEDIATE(x) || !SCM_BIGP(x))
	goto badx;
      if (SCM_INUMP(y))
	{SCM t = x; x = y; y = t; goto intbig;}
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	goto bady;
      return scm_mulbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y));
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_product);
#  endif
    intbig: if (SCM_INUM0==x) return x; if (SCM_MAKINUM(1L)==x) return y;
      {
#  ifndef SCM_DIGSTOOBIG
	long z = scm_pseudolong(SCM_INUM(x));
	return scm_mulbig(&z, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			  SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  else
	SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	scm_longdigs(SCM_INUM(x), zdigs);
	return scm_mulbig(zdigs, SCM_DIGSPERLONG, SCM_BDIGITS(y), SCM_NUMDIGS(y),
			  SCM_BIGSIGN(y) ? (x>0) : (x<0));
#  endif
      }
    }
# else
  if (!SCM_INUMP(x))
    goto badx;
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_product);
# endif
#endif
  {
    long i, j, k;
    i = SCM_INUM(x);
    if (0==i) return x;
    j = SCM_INUM(y);
    k = i * j;
    y = SCM_MAKINUM(k);
    if (k != SCM_INUM(y) || k/i != j)
#ifdef SCM_BIGDIG
      { int sgn = (i < 0) ^ (j < 0);
# ifndef SCM_DIGSTOOBIG
	i = scm_pseudolong(i);
	j = scm_pseudolong(j);
	return scm_mulbig((SCM_BIGDIG *)&i, SCM_DIGSPERLONG,
			  (SCM_BIGDIG *)&j, SCM_DIGSPERLONG, sgn);
# else /* SCM_DIGSTOOBIG */
	SCM_BIGDIG idigs[SCM_DIGSPERLONG];
	SCM_BIGDIG jdigs[SCM_DIGSPERLONG];
	scm_longdigs(i, idigs);
	scm_longdigs(j, jdigs);
	return scm_mulbig(idigs, SCM_DIGSPERLONG, jdigs, SCM_DIGSPERLONG, sgn);
# endif
      }
#else
# ifdef SCM_FLOATS
    return scm_makdbl(((double)i)*((double)j), 0.0);
# else
    scm_wta(y, scm_ovflow, s_product);
# endif
#endif
    return y;
  }
}


double
scm_num2dbl (SCM a, SCM msg, SCM why)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP (a))
    return (double) SCM_INUM (a);
#ifdef SCM_FLOATS
  SCM_ASSERT (!SCM_IS_IMMEDIATE (a), a, msg, why);
  if (SCM_REALP (a))
    return (SCM_REALPART (a));
#endif
#ifdef SCM_BIGDIG
  return scm_big2dbl (a);
#endif
  SCM_ASSERT (0, a, msg, why);
  return SCM_UNSPECIFIED;
}


SCM_PROC1 (s_divide, "/", scm_tc7_asubr, scm_divide);
SCM
scm_divide(SCM x, SCM y)
{
  SCM_INTS_ENABLED;

#ifdef SCM_FLOATS
  double d, r, i, a;
  if (SCM_NINUMP(x))
    {
# ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(x)))
      badx: scm_wta(x, scm_arg1, s_divide);
# endif
      if (SCM_UNBNDP(y))
	{
# ifdef SCM_BIGDIG
	  if (SCM_BIGP(x))
	    return scm_makdbl(1.0/scm_big2dbl(x), 0.0);
# endif
	  if (!SCM_INEXP(x))
	    goto badx;
	  if (SCM_REALP(x))
	    return scm_makdbl(1.0/SCM_REALPART(x), 0.0);
	  r = SCM_REAL(x);  i = SCM_IMAG(x);  d = r*r+i*i;
	  return scm_makdbl(r/d, -i/d);
	}
# ifdef SCM_BIGDIG
      if (SCM_BIGP(x))
	{
	  SCM z;
	  if (SCM_INUMP(y))
	    {
	      z = SCM_INUM(y);
	      SCM_ASSERT(z, y, scm_ovflow, s_divide);
	      if (1==z) return x;
	      if (z < 0) z = -z;
	      if (z < SCM_BIGRAD) {
				    SCM w = scm_copybig(x, SCM_BIGSIGN(x) ? (y>0) : (y<0));
				    return scm_divbigdig(SCM_BDIGITS(w), SCM_NUMDIGS(w), (SCM_BIGDIG)z) ?
				      scm_makdbl(scm_big2dbl(x)/SCM_INUM(y), 0.0) : scm_normbig(w);
				  }
#  ifndef SCM_DIGSTOOBIG
	      z = scm_pseudolong(z);
	      z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), (SCM_BIGDIG *)&z, SCM_DIGSPERLONG,
				SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);
#  else
	      { SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
		scm_longdigs(z, zdigs);
		z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), zdigs, SCM_DIGSPERLONG,
				  SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);}
#  endif
	      return z ? z : scm_makdbl(scm_big2dbl(x)/SCM_INUM(y), 0.0);
	    }
	  if (!!SCM_IS_IMMEDIATE(y))
	    goto bady;
	  if (SCM_BIGP(y))
	    {
	      z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
				SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y), 3);
	      return z ? z : scm_makdbl(scm_big2dbl(x)/scm_big2dbl(y), 0.0);
	    }
	  if (!SCM_INEXP(y))
	    goto bady;
	  if (SCM_REALP(y))
	    return scm_makdbl(scm_big2dbl(x)/SCM_REALPART(y), 0.0);
	  a = scm_big2dbl(x);
	  goto complex_div;
	}
# endif
      if (!SCM_INEXP(x))
	goto badx;
      if (SCM_INUMP(y))
	{d = SCM_INUM(y); goto basic_div;}
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	{d = scm_big2dbl(y); goto basic_div;}
      if (!SCM_INEXP(y))
	goto bady;
# else
      if (!!SCM_IS_IMMEDIATE(y) || !SCM_INEXP(y))
	goto bady;
# endif
      if (SCM_REALP(y))
	{
	  d = SCM_REALPART(y);
	basic_div: return scm_makdbl(SCM_REALPART(x)/d, SCM_CPLXP(x)?SCM_IMAG(x)/d:0.0);
	}
      a = SCM_REALPART(x);
      if (SCM_REALP(x))
	goto complex_div;
      r = SCM_REAL(y);  i = SCM_IMAG(y);  d = r*r+i*i;
      return scm_makdbl((a*r+SCM_IMAG(x)*i)/d, (SCM_IMAG(x)*r-a*i)/d);
    }
  if (SCM_UNBNDP(y))
    {
      if ((SCM_MAKINUM(1L)==x) || (SCM_MAKINUM(-1L)==x)) return x;
      return scm_makdbl(1.0/((double)SCM_INUM(x)), 0.0);
    }
  if (SCM_NINUMP(y))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(y))
	goto bady;
      if (SCM_BIGP(y))
	return scm_makdbl(SCM_INUM(x)/scm_big2dbl(y), 0.0);
#  ifndef SCM_RECKLESS
      if (!(SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_divide);
#  endif
# else
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_INEXP(y)))
      bady: scm_wta(y, scm_arg2, s_divide);
#  endif
# endif
      if (SCM_REALP(y))
	return scm_makdbl(SCM_INUM(x)/SCM_REALPART(y), 0.0);
      a = SCM_INUM(x);
    complex_div:
      r = SCM_REAL(y);  i = SCM_IMAG(y);  d = r*r+i*i;
      return scm_makdbl((a*r)/d, (-a*i)/d);
    }
#else
# ifdef SCM_BIGDIG
  if (SCM_NINUMP(x))
    {
      SCM z;
      SCM_ASSERT(!SCM_IS_IMMEDIATE(x) && SCM_BIGP(x), x, scm_arg1, s_divide);
      if (SCM_UNBNDP(y))
	goto ov;
      if (SCM_INUMP(y))
	{
	  z = SCM_INUM(y);
	  if (!z) goto ov;
	  if (1==z) return x;
	  if (z < 0) z = -z;
	  if (z < SCM_BIGRAD) {
				SCM w = scm_copybig(x, SCM_BIGSIGN(x) ? (y>0) : (y<0));
				if (scm_divbigdig(SCM_BDIGITS(w), SCM_NUMDIGS(w), (SCM_BIGDIG)z)) goto ov;
				return w;
			      }
#  ifndef SCM_DIGSTOOBIG
	  z = scm_pseudolong(z);
	  z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), &z, SCM_DIGSPERLONG,
			    SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);
#  else
	  { SCM_BIGDIG zdigs[SCM_DIGSPERLONG];
	    scm_longdigs(z, zdigs);
	    z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), zdigs, SCM_DIGSPERLONG,
			      SCM_BIGSIGN(x) ? (y>0) : (y<0), 3);}
#  endif
	}
      else
	{
	  if (!!SCM_IS_IMMEDIATE(y) || !SCM_BIGP(y))
	    goto bady;
	  z = scm_divbigbig(SCM_BDIGITS(x), SCM_NUMDIGS(x), SCM_BDIGITS(y), SCM_NUMDIGS(y),
			    SCM_BIGSIGN(x) ^ SCM_BIGSIGN(y), 3);
	}
      if (!z) goto ov;
      return z;
    }
  if (SCM_UNBNDP(y))
    {
      if ((SCM_MAKINUM(1L)==x) || (SCM_MAKINUM(-1L)==x))
	return x;
      goto ov;
    }
  if (SCM_NINUMP(y))
    {
#  ifndef SCM_RECKLESS
      if (!(!SCM_IS_IMMEDIATE(y) && SCM_BIGP(y)))
      bady: scm_wta(y, scm_arg2, s_divide);
#  endif
      goto ov;
    }
# else
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_divide);
  if (SCM_UNBNDP(y))
    {
      if ((SCM_MAKINUM(1L)==x) || (SCM_MAKINUM(-1L)==x)) return x;
      goto ov;
    }
  SCM_ASSERT(SCM_INUMP(y), y, scm_arg2, s_divide);
# endif
#endif
  {
    long z = SCM_INUM(y);
    if ((0==z) || SCM_INUM(x)%z) goto ov;
    z = SCM_INUM(x)/z;
    if (SCM_FIXABLE(z))
      return SCM_MAKINUM(z);
#ifdef SCM_BIGDIG
    return scm_long2big(z);
#endif
#ifdef SCM_FLOATS
  ov: return scm_makdbl(((double)SCM_INUM(x))/((double)SCM_INUM(y)), 0.0);
#else
  ov: scm_wta(x, scm_ovflow, s_divide);
    return SCM_UNSPECIFIED;
#endif
  }
}




#ifdef SCM_FLOATS
SCM_PROC1 (s_asinh, "$asinh", scm_tc7_cxr, (SCM (*)()) scm_asinh);
double
scm_asinh(double x)
{
  SCM_INTS_DISABLED;

  return log(x+sqrt(x*x+1));
}




SCM_PROC1 (s_acosh, "$acosh", scm_tc7_cxr, (SCM (*)()) scm_acosh);
double
scm_acosh(double x)
{
  SCM_INTS_DISABLED;

  return log(x+sqrt(x*x-1));
}




SCM_PROC1 (s_atanh, "$atanh", scm_tc7_cxr, (SCM (*)()) scm_atanh);
double
scm_atanh(double x)
{
  SCM_INTS_DISABLED;

  return 0.5*log((1+x)/(1-x));
}




SCM_PROC1 (s_truncate, "truncate", scm_tc7_cxr, (SCM (*)()) scm_truncate);
double
scm_truncate(double x)
{
  SCM_INTS_DISABLED;

  if (x < 0.0) return -floor(-x);
  return floor(x);
}



SCM_PROC1 (s_round, "round", scm_tc7_cxr, (SCM (*)()) scm_round);
double
scm_round(double x)
{
  SCM_INTS_DISABLED;

  double plus_half = x + 0.5;
  double result = floor(plus_half);
  /* Adjust so that the scm_round is towards even.  */
  return (plus_half == result && plus_half / 2 != floor(plus_half / 2))
    ? result - 1 : result;
}



SCM_PROC1 (s_exact_to_inexact, "exact->inexact", scm_tc7_cxr, (SCM (*)()) scm_exact_to_inexact);
double
scm_exact_to_inexact(double z)
{
  SCM_INTS_INDIFFERENT;

  return z;
}


SCM_PROC1 (s_i_floor, "floor", scm_tc7_cxr, (SCM (*)()) floor);
SCM_PROC1 (s_i_ceil, "ceiling", scm_tc7_cxr, (SCM (*)()) ceil);
SCM_PROC1 (s_i_sqrt, "$sqrt", scm_tc7_cxr, (SCM (*)())sqrt);
SCM_PROC1 (s_i_abs, "$abs", scm_tc7_cxr, (SCM (*)())fabs);
SCM_PROC1 (s_i_exp, "$exp", scm_tc7_cxr, (SCM (*)())exp);
SCM_PROC1 (s_i_log, "$log", scm_tc7_cxr, (SCM (*)())log);
SCM_PROC1 (s_i_sin, "$sin", scm_tc7_cxr, (SCM (*)())sin);
SCM_PROC1 (s_i_cos, "$cos", scm_tc7_cxr, (SCM (*)())cos);
SCM_PROC1 (s_i_tan, "$tan", scm_tc7_cxr, (SCM (*)())tan);
SCM_PROC1 (s_i_asin, "$asin", scm_tc7_cxr, (SCM (*)())asin);
SCM_PROC1 (s_i_acos, "$acos", scm_tc7_cxr, (SCM (*)())acos);
SCM_PROC1 (s_i_atan, "$atan", scm_tc7_cxr, (SCM (*)())atan);
SCM_PROC1 (s_i_sinh, "$sinh", scm_tc7_cxr, (SCM (*)())sinh);
SCM_PROC1 (s_i_cosh, "$cosh", scm_tc7_cxr, (SCM (*)())cosh);
SCM_PROC1 (s_i_tanh, "$tanh", scm_tc7_cxr, (SCM (*)())tanh);

void
scm_two_doubles(SCM z1, SCM z2, SCM sstring, struct scm_dpair *xy)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP(z1))
    xy->x = SCM_INUM(z1);
  else
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(z1))
	goto badz1;
      if (SCM_BIGP(z1))
	xy->x = scm_big2dbl(z1);
      else {
#  ifndef SCM_RECKLESS
	     if (!(SCM_REALP(z1)))
	     badz1: scm_wta(z1, scm_arg1, sstring);
#  endif
	     xy->x = SCM_REALPART(z1);}
# else
      {SCM_ASSERT(!SCM_IS_IMMEDIATE(z1) && SCM_REALP(z1), z1, scm_arg1, sstring);
	xy->x = SCM_REALPART(z1);}
# endif
    }
  if (SCM_INUMP(z2))
    xy->y = SCM_INUM(z2);
  else
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(z2))
	goto badz2;
      if (SCM_BIGP(z2))
	xy->y = scm_big2dbl(z2);
      else {
#  ifndef SCM_RECKLESS
	     if (!(SCM_REALP(z2)))
	     badz2: scm_wta(z2, scm_arg2, sstring);
#  endif
	     xy->y = SCM_REALPART(z2);}
# else
      {SCM_ASSERT(!SCM_IS_IMMEDIATE(z2) && SCM_REALP(z2), z2, scm_arg2, sstring);
	xy->y = SCM_REALPART(z2);}
# endif
    }
}




SCM_PROC (s_expt, "$expt", 2, 0, 0, scm_expt);
SCM
scm_expt(SCM z1, SCM z2)
{
  SCM_INTS_ENABLED;

  double p;
  struct scm_dpair xy;
  scm_two_doubles(z1, z2, s_expt, &xy);
  SCM_DEFER_INTS;
  p = pow(xy.x, xy.y);
  SCM_ALLOW_INTS;
  return scm_makdbl(p, 0.0);
}



SCM_PROC(s_atan2, "$atan2", 2, 0, 0, scm_atan2);
SCM
scm_atan2(SCM z1, SCM z2)
{
  SCM_INTS_ENABLED;

  struct scm_dpair xy;
  double a;
  scm_two_doubles(z1, z2, s_atan2, &xy);
  SCM_DEFER_INTS;
  a = atan2(xy.x, xy.y);
  SCM_ALLOW_INTS;
  return scm_makdbl(a, 0.0);
}



SCM_PROC(s_make_rectangular, "make-rectangular", 2, 0, 0, scm_make_rectangular);
SCM
scm_make_rectangular(SCM z1, SCM z2)
{
  SCM_INTS_ENABLED;

  struct scm_dpair xy;
  scm_two_doubles(z1, z2, s_make_rectangular, &xy);
  return scm_makdbl(xy.x, xy.y);
}



SCM_PROC(s_make_polar, "make-polar", 2, 0, 0, scm_make_polar);
SCM
scm_make_polar(SCM z1, SCM z2)
{
  SCM_INTS_ENABLED;

  struct scm_dpair xy;
  scm_two_doubles(z1, z2, s_make_polar, &xy);
  {
    double r;
    double i;
    SCM_DEFER_INTS;
    r = xy.x*cos(xy.y);
    i = xy.x*sin(xy.y);
    SCM_ALLOW_INTS;
    return scm_makdbl(r, i);
  }
}




SCM_PROC(s_real_part, "real-part", 1, 0, 0, scm_real_part);
SCM
scm_real_part(SCM z)
{
  SCM_INTS_ENABLED;

  if (SCM_NINUMP(z))
    {
# ifdef SCM_BIGDIG
      if (!!SCM_IS_IMMEDIATE(z))
	goto badz;
      if (SCM_BIGP(z))
	return z;
#  ifndef SCM_RECKLESS
      if (!(SCM_INEXP(z)))
      badz: scm_wta(z, scm_arg1, s_real_part);
#  endif
# else
      SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_INEXP(z), z, scm_arg1, s_real_part);
# endif
      if (SCM_CPLXP(z))
	return scm_makdbl(SCM_REAL(z), 0.0);
    }
  return z;
}



SCM_PROC(s_imag_part, "imag-part", 1, 0, 0, scm_imag_part);
SCM
scm_imag_part(SCM z)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP(z))
    return SCM_INUM0;
# ifdef SCM_BIGDIG
  if (!!SCM_IS_IMMEDIATE(z))
    goto badz;
  if (SCM_BIGP(z))
    return SCM_INUM0;
#  ifndef SCM_RECKLESS
  if (!(SCM_INEXP(z)))
  badz: scm_wta(z, scm_arg1, s_imag_part);
#  endif
# else
  SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_INEXP(z), z, scm_arg1, s_imag_part);
# endif
  if (SCM_CPLXP(z))
    return scm_makdbl(SCM_IMAG(z), 0.0);
  return scm_flo0;
}



SCM_PROC(s_angle, "angle", 1, 0, 0, scm_angle);
SCM
scm_angle(SCM z)
{
  SCM_INTS_ENABLED;

  double x, y = 0.0;
  if (SCM_INUMP(z))
    {x = (z>=SCM_INUM0) ? 1.0 : -1.0; goto do_angle;}
# ifdef SCM_BIGDIG
  if (!!SCM_IS_IMMEDIATE(z))
    goto badz;
  if (SCM_BIGP(z))
    {x = (SCM_TYP16(z)==scm_tc16_bigpos) ? 1.0 : -1.0; goto do_angle;}
#  ifndef SCM_RECKLESS
  if (!(SCM_INEXP(z))) {
  badz: scm_wta(z, scm_arg1, s_angle);}
#  endif
# else
  SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_INEXP(z), z, scm_arg1, s_angle);
# endif
  if (SCM_REALP(z))
    {
      x = SCM_REALPART(z);
      goto do_angle;
    }
  x = SCM_REAL(z); y = SCM_IMAG(z);
 do_angle:
  {
    double a;
    SCM_DEFER_INTS;
    a = atan2(y, x);
    SCM_ALLOW_INTS;
    return scm_makdbl(a, 0.0);
  }
}


SCM_PROC(s_inexact_to_exact, "inexact->exact", 1, 0, 0, scm_inexact_to_exact);
SCM
scm_inexact_to_exact(SCM z)
{
  SCM_INTS_ENABLED;

  if (SCM_INUMP(z))
    return z;
# ifdef SCM_BIGDIG
  if (!!SCM_IS_IMMEDIATE(z))
    goto badz;
  if (SCM_BIGP(z))
    return z;
#  ifndef SCM_RECKLESS
  if (!(SCM_REALP(z)))
  badz: scm_wta(z, scm_arg1, s_inexact_to_exact);
#  endif
# else
  SCM_ASSERT(!SCM_IS_IMMEDIATE(z) && SCM_REALP(z), z, scm_arg1, s_inexact_to_exact);
# endif
# ifdef SCM_BIGDIG
  {
    double u = floor(SCM_REALPART(z)+0.5);
    if ((u <= SCM_MOST_POSITIVE_FIXNUM) && (-u <= -SCM_MOST_NEGATIVE_FIXNUM)) {
      /* Negation is a workaround for HP700 cc bug */
      SCM ans = SCM_MAKINUM((long)u);
      if (SCM_INUM(ans)==(long)u) return ans;
    }
    if (IS_INF(u))
      goto badz;	/* problem? */
    return scm_dbl2big(u);
  }
# else
  return SCM_MAKINUM((long)floor(SCM_REALPART(z)+0.5));
# endif
}



#else				/* ~SCM_FLOATS */
SCM_PROC(s_trunc, "truncate", 1, 0, 0, scm_trunc);
SCM
scm_trunc(SCM x)
{
  SCM_ASSERT(SCM_INUMP(x), x, scm_arg1, s_truncate);
  return x;
}



#endif				/* SCM_FLOATS */

#ifdef SCM_BIGDIG
# ifdef SCM_FLOATS
/* d must be integer */
SCM
scm_dbl2big(double d)
{
  SCM_INTS_ENABLED;

  size_t i = 0;
  long c;
  SCM_BIGDIG *digits;
  SCM ans;
  double u = (d < 0)?-d:d;
  SCM_DEFER_INTS;
  while (0 != floor(u)) {u /= SCM_BIGRAD;i++;}
  SCM_ALLOW_INTS;
  ans = scm_mkbig(i, d < 0);
  digits = SCM_BDIGITS(ans);
  SCM_DEFER_INTS;
  while (i--)
    {
      u *= SCM_BIGRAD;
      c = floor(u);
      u -= c;
      digits[i] = c;
    }
  SCM_ALLOW_INTS;
  SCM_ASSERT(0==u, SCM_INUM0, scm_ovflow, s_dbl2big);
  return ans;
}



double
scm_big2dbl(SCM b)
{
  SCM_INTS_INDIFFERENT;

  double ans = 0.0;
  size_t i = SCM_NUMDIGS(b);
  SCM_BIGDIG *digits = SCM_BDIGITS(b);
  while (i--) ans = digits[i] + SCM_BIGRAD*ans;
  if (scm_tc16_bigneg==SCM_TYP16(b)) return -ans;
  return ans;
}
# endif
#endif

SCM
scm_long2num(long sl)
{
  SCM_INTS_NESTED;

  if (!SCM_FIXABLE(sl)) {
#ifdef SCM_BIGDIG
    return scm_long2big(sl);
#else
# ifdef SCM_FLOATS
    return scm_makdbl((double) sl, 0.0);
# else
    return SCM_BOOL_F;
# endif
#endif
  }
  return SCM_MAKINUM(sl);
}



SCM
scm_ulong2num(unsigned long sl)
{
  SCM_INTS_NESTED;

  if (!SCM_POSSCM_FIXABLE(sl)) {
#ifdef SCM_BIGDIG
    return scm_ulong2big(sl);
#else
# ifdef SCM_FLOATS
    return scm_makdbl((double) sl, 0.0);
# else
    return SCM_BOOL_F;
# endif
#endif
  }
  return SCM_MAKINUM(sl);
}

long
scm_num2long(SCM num, SCM pos, SCM s_caller)
{
  SCM_INTS_ENABLED;

  long res;
  if (SCM_INUMP(num))
    {
      res = SCM_INUM(num);
      return res;
    }
  if (!!SCM_IS_IMMEDIATE(num))
    goto errout;
#ifdef SCM_FLOATS
  if (SCM_REALP(num))
    {
      double u = SCM_REALPART(num);
      res = u;
      if ((double)res == u)
	{
	  return res;
	}
    }
#endif
#ifdef SCM_BIGDIG
  if (SCM_BIGP(num)) {
    long oldres;
    size_t l;
    res = 0;
    oldres = 0;
    for(l = SCM_NUMDIGS(num);l--;)
      {
	res = SCM_BIGUP(res) + SCM_BDIGITS(num)[l];
	if (res < oldres)
	  goto errout;
	oldres = res;
      }
    if (SCM_TYP16 (num) == scm_tc16_bigpos)
      return res;
    else
      return -res;
  }
#endif
 errout: scm_wta(num, pos, s_caller);
  return SCM_UNSPECIFIED;
}




long
num2long(SCM num, SCM pos, SCM s_caller)
{
  SCM_INTS_ENABLED;

  long res;
  if (SCM_INUMP(num))
    {
      res = SCM_INUM((long)num);
      return res;
    }
  if (!!SCM_IS_IMMEDIATE(num))
    goto errout;
#ifdef SCM_FLOATS
  if (SCM_REALP(num))
    {
      double u = SCM_REALPART(num);
      if (((SCM_MOST_NEGATIVE_FIXNUM * 4) <= u)
	  && (u <= (SCM_MOST_POSITIVE_FIXNUM * 4 + 3)))
	{
	  res = u;
	  return res;
	}
    }
#endif
#ifdef SCM_BIGDIG
  if (SCM_BIGP(num))
    {
      size_t l = SCM_NUMDIGS(num);
      if (SCM_DIGSPERLONG < l)
	goto errout;
      res = 0;
      for(;l--;) res = SCM_BIGUP(res) + SCM_BDIGITS(num)[l];
      return res;
    }
#endif
 errout: scm_wta(num, pos, s_caller);
  return SCM_UNSPECIFIED;
}




unsigned long
scm_num2ulong_maybe (int * okp, SCM num)
{
  SCM_INTS_INDIFFERENT;
  unsigned long res;

  *okp = 1;
  if (SCM_INUMP(num))
    {
      res = SCM_INUM((unsigned long)num);
      return res;
    }
  if (!!SCM_IS_IMMEDIATE (num))
    {
      *okp = 0;
      return 0;
    }

#ifdef SCM_FLOATS
  if (SCM_REALP(num))
    {
      double u = SCM_REALPART(num);
      if ((0 <= u) && (u <= (unsigned long)~0L))
	{
	  res = u;
	  return res;
	}
    }
#endif
#ifdef SCM_BIGDIG
  if (SCM_BIGP(num)) {
    unsigned long oldres;
    size_t l;
    res = 0;
    oldres = 0;
    for(l = SCM_NUMDIGS(num);l--;)
      {
	res = SCM_BIGUP(res) + SCM_BDIGITS(num)[l];
	if (res < oldres)
	  {
	    *okp = 0;
	    return 0;
	  }
	oldres = res;
      }
    return res;
  }
#endif
  *okp = 0;
  return 0;
}


unsigned long
scm_num2ulong (SCM num, SCM msg, SCM s_caller)
{
  int okp;
  SCM answer;

  answer = scm_num2ulong_maybe (&okp, num);
  if (!okp)
    scm_wta (num, msg, s_caller);
  return answer;
}


#ifdef SCM_FLOATS
# ifndef DBL_DIG
static void add1(f, fsum)
     double f, *fsum;
{
  *fsum = f + 1.0;
}
# endif
#endif


void
scm_init_numbers (void)
{
  SCM_INTS_DISABLED;

#ifdef SCM_FLOATS
  SCM_NEWCELL(scm_flo0);
# ifdef SCM_SINGLES
  SCM_CAR(scm_flo0) = scm_tc16_flo;
  SCM_FLO(scm_flo0) = 0.0;
# else
  SCM_CDR(scm_flo0) = (SCM)scm_must_malloc(1L*sizeof(double));
  SCM_REAL(scm_flo0) = 0.0;
  SCM_CAR(scm_flo0) = scm_tc_dblr;
# endif
# ifdef DBL_DIG
  scm_dblprec = (DBL_DIG > 20) ? 20 : DBL_DIG;
# else
  {				/* determine floating point precision */
    double f = 0.1;
    double fsum = 1.0+f;
    while (fsum != 1.0) {
      f /= 10.0;
      if (++scm_dblprec > 20) break;
      add1(f, &fsum);
    }
    scm_dblprec = scm_dblprec-1;
  }
# endif /* DBL_DIG */
#endif

  scm_intern_symhash ("most-positive-fixnum", (SCM) SCM_MAKINUM (SCM_MOST_POSITIVE_FIXNUM));
  scm_intern_symhash ("most-negative-fixnum", (SCM) SCM_MAKINUM (SCM_MOST_NEGATIVE_FIXNUM));

#include "systas/libsystas/numbers.x"
}

