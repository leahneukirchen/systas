/* tag: Tom Lord Tue Dec  4 14:41:53 2001 (numbers.h)
 */

#ifndef INCLUDE__LIBSYSTAS__NUMBERS_H
#define INCLUDE__LIBSYSTAS__NUMBERS_H
/* Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 *
 */


#include "systas/libsystas/scm.h"
#include "systas/libsystas/strings.h"


struct scm_dpair
{
  double x;
  double y;
};



/* Immediate Numbers 
 *
 * Inums are exact integer data that fits within an SCM word.
 *
 * SCM_INUMP applies only to values known to be Scheme objects.
 * In particular, SCM_INUMP (SCM_CAR (x)) is valid only if x is known
 * to be a SCM_CONSP.  If x is only known to be a !SCM_IS_IMMEDIATE, 
 * SCM_INUMP (SCM_CAR (x)) can give wrong answers.
 */

#define SCM_INUMP(x)	(2 & (int)(x))
#define SCM_NINUMP(x) 	(!SCM_INUMP(x))

#ifdef __TURBOC__
/* shifts of more than one are done by a library call, single shifts are
 * performed in registers
 */
# define SCM_MAKINUM(x) ((((x)<<1)<<1)+2L)
#else
# define SCM_MAKINUM(x) (((x)<<2)+2L)
#endif /* def __TURBOC__ */


/* SCM_SRS is signed right shift */
/* Turbo C++ v1.0 has a bug with right shifts of signed longs!
 * It is believed to be fixed in Turbo C++ v1.01
 */
#if (-1==(((-1)<<2)+2)>>2) && (__TURBOC__ != 0x295)
# define SCM_SRS(x, y) ((x)>>y)
# ifdef __TURBOC__
#  define SCM_INUM(x) (((x)>>1)>>1)
# else
#  define SCM_INUM(x) SCM_SRS(x, 2)
# endif /* def __TURBOC__ */
#else
# define SCM_SRS(x, y) (((x)<0) ? ~((~(x))>>y) : (x)>>y)
# define SCM_INUM(x) SCM_SRS(x, 2)
#endif /*  (-1==(((-1)<<2)+2)>>2) && (__TURBOC__ != 0x295) */


/* A name for 0.
 */
#define SCM_INUM0 ((SCM) 2)



/* SCM_FIXABLE is non-0 if its long argument can be encoded in an SCM_INUM.
 */
#define SCM_MOST_POSITIVE_FIXNUM (LONG_MAX>>2)
#define SCM_MOST_NEGATIVE_FIXNUM SCM_SRS((long)LONG_MIN, 2)
#define SCM_POSSCM_FIXABLE(n) ((n) <= SCM_MOST_POSITIVE_FIXNUM)
#define SCM_NEGSCM_FIXABLE(n) ((n) >= SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_UNEGSCM_FIXABLE(n) ((n) <= -SCM_MOST_NEGATIVE_FIXNUM)
#define SCM_FIXABLE(n) (SCM_POSSCM_FIXABLE(n) && SCM_NEGSCM_FIXABLE(n))

/* SCM_INTBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an exact immediate.
 */

#ifndef SCM_CHAR_BIT
# define SCM_CHAR_BIT 8
#endif /* ndef SCM_CHAR_BIT */
#ifndef SCM_LONG_BIT
# define SCM_LONG_BIT (SCM_CHAR_BIT*sizeof(long)/sizeof(char))
#endif /* ndef SCM_LONG_BIT */
#define SCM_INTBUFLEN (5+SCM_LONG_BIT)

/* SCM_FLOBUFLEN is the maximum number of characters neccessary for the
 * printed or scm_string representation of an inexact number.
 */

#define SCM_FLOBUFLEN (10+2*(sizeof(double)/sizeof(char)*SCM_CHAR_BIT*3+9)/10)




/* Numbers 
 */

#define SCM_INEXP(x) (SCM_TYP16(x)==scm_tc16_flo)
#define SCM_CPLXP(x) (SCM_CAR(x)==scm_tc_dblc)
#define SCM_REAL(x) (*(((scm_dbl *) (x))->real))
#define SCM_IMAG(x) (*((double *)(SCM_CDR(x)+sizeof(double))))
/* ((&SCM_REAL(x))[1]) */


#ifdef SCM_SINGLES
#define SCM_REALP(x) ((~SCM_REAL_PART & SCM_CAR(x))==scm_tc16_flo)
#define SCM_SINGP(x) (SCM_CAR(x)==scm_tc16_flo)
#define SCM_FLO(x) (((scm_flo *)(x))->num)
#define SCM_REALPART(x) (SCM_SINGP(x)?0.0+SCM_FLO(x):SCM_REAL(x))
#else /* SCM_SINGLES */
#define SCM_REALP(x) (SCM_CAR(x)==scm_tc_dblr)
#define SCM_REALPART SCM_REAL
#endif /* SCM_SINGLES */


/* Define SCM_BIGDIG to an integer type whose size is smaller than long if
 * you want bignums.  SCM_BIGRAD is one greater than the biggest SCM_BIGDIG. 
 *
 * Define SCM_DIGSTOOBIG if the digits equivalent to a long won't fit in a long. 
 */
#ifdef SCM_BIGNUMS
# ifdef _UNICOS
#  define SCM_DIGSTOOBIG
#  if (1L << 31) <= SCM_USHRT_MAX
#   define SCM_BIGDIG unsigned  short
#  else
#   define SCM_BIGDIG unsigned int
#  endif /*  (1L << 31) <= USHRT_MAX */
#  define SCM_BITSPERDIG 32
# else
#  define SCM_BIGDIG unsigned short
#  define SCM_BITSPERDIG (sizeof(SCM_BIGDIG)*SCM_CHAR_BIT)
# endif /* def _UNICOS */

# define SCM_BIGRAD (1L << SCM_BITSPERDIG)
# define SCM_DIGSPERLONG ((size_t)((sizeof(long)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))
# define SCM_DIGSPERLONGLONG ((size_t)((sizeof(long long)*SCM_CHAR_BIT+SCM_BITSPERDIG-1)/SCM_BITSPERDIG))
# define SCM_BIGUP(x) ((unsigned long)(x) << SCM_BITSPERDIG)
# define SCM_LONGLONGSCM_BIGUP(x) ((ulong_long)(x) << SCM_BITSPERDIG)
# define SCM_BIGDN(x) ((x) >> SCM_BITSPERDIG)
# define SCM_BIGLO(x) ((x) & (SCM_BIGRAD-1))
#endif /* def SCM_BIGNUMS */

#ifndef SCM_BIGDIG
/* Definition is not really used but helps various function
 * prototypes to compile with conditionalization.
 */
# define SCM_BIGDIG unsigned short
# define NO_SCM_BIGDIG
# ifndef SCM_FLOATS
#  define SCM_INUMS_ONLY
# endif /* ndef SCM_FLOATS */
#endif /* ndef SCM_BIGDIG */

#ifdef SCM_FLOATS
#define SCM_NUMBERP(x) (SCM_INUMP(x) || (!SCM_IS_IMMEDIATE(x) && SCM_NUMP(x)))
#else
#ifdef SCM_BIGDIG
#define SCM_NUMBERP(x) (SCM_INUMP(x) || (!SCM_IS_IMMEDIATE(x) && SCM_NUMP(x)))
#else
#define SCM_NUMBERP SCM_INUMP
#endif
#endif
#define SCM_NUMP(x) ((0xfcff & (int)SCM_CAR(x))==scm_tc7_smob)
#define SCM_BIGP(x) (SCM_TYP16S(x)==scm_tc16_bigpos)
#define SCM_BIGSIGN(x) (0x0100 & (int)SCM_CAR(x))
#define SCM_BDIGITS(x) ((SCM_BIGDIG *)(SCM_CDR(x)))
#define SCM_NUMDIGS(x) ((size_t)(SCM_CAR(x)>>16))
#define SCM_SETNUMDIGS(x, v, t) SCM_CAR(x) = (((v)+0L)<<16)+(t)


#ifdef SCM_FLOATS
typedef struct scm_dblproc
{
  char *scm_string;
  double (*cproc) ();
} scm_dblproc;

#ifdef SCM_SINGLES
typedef struct scm_flo
{
  SCM type;
  float num;
} scm_flo;
#endif

typedef struct scm_dbl
{
  SCM type;
  double *real;
} scm_dbl;
#endif




/* automatically generated __STDC__ prototypes */
extern SCM scm_random32 ();
extern SCM scm_random32_seed (SCM seed);
extern SCM scm_exact_p(SCM x);
extern SCM scm_odd_p(SCM n);
extern SCM scm_even_p(SCM n);
extern SCM scm_magnitude(SCM z);
extern SCM scm_quotient(SCM x, SCM y);
extern SCM scm_remainder(SCM x, SCM y);
extern SCM scm_modulo(SCM x, SCM y);
extern SCM scm_gcd(SCM x, SCM y);
extern SCM scm_lcm(SCM n1, SCM n2);
extern SCM scm_logand(SCM n1, SCM n2);
extern SCM scm_logior(SCM n1, SCM n2);
extern SCM scm_logxor(SCM n1, SCM n2);
extern SCM scm_logtest(SCM n1, SCM n2);
extern SCM scm_logbit_p(SCM n1, SCM n2);
extern SCM scm_logand(SCM n1, SCM n2);
extern SCM scm_logior(SCM n1, SCM n2);
extern SCM scm_logxor(SCM n1, SCM n2);
extern SCM scm_logtest(SCM n1, SCM n2);
extern SCM scm_logbit_p(SCM n1, SCM n2);
extern SCM scm_lognot(SCM n);
extern SCM scm_integer_expt(SCM z1, SCM z2);
extern SCM scm_ash(SCM n, SCM cnt);
extern SCM scm_bit_extract(SCM n, SCM start, SCM end);
extern SCM scm_logcount (SCM n);
extern SCM scm_integer_length(SCM n);
extern SCM scm_mkbig(size_t nlen, int sign);
extern SCM scm_big2inum(SCM b, size_t l);
extern SCM scm_adjbig(SCM b, size_t nlen);
extern SCM scm_normbig(SCM b);
extern SCM scm_copybig(SCM b, int sign);
extern SCM scm_long2big(long n);
extern SCM scm_2ulong2big(unsigned long * np);
extern SCM scm_ulong2big(unsigned long n);
extern int scm_bigcomp(SCM x, SCM y);
extern long scm_pseudolong(long x);
extern void scm_longdigs(long x, SCM_BIGDIG digs[]);
extern SCM scm_addbig(SCM_BIGDIG *x, size_t nx, int xsgn, SCM bigy, int sgny);
extern SCM scm_mulbig(SCM_BIGDIG *x, size_t nx, SCM_BIGDIG *y, size_t ny, int sgn);
extern unsigned int scm_divbigdig(SCM_BIGDIG *ds, size_t h, SCM_BIGDIG dv);
extern SCM scm_divbigint(SCM x, long z, int sgn, int mode);
extern SCM scm_divbigbig(SCM_BIGDIG *x, size_t nx, SCM_BIGDIG *y, size_t ny, int sgn, int modes);
extern size_t scm_iint2str(long num, int rad, char *p);
extern SCM scm_number_to_string(SCM x, SCM radix);
extern int scm_floprint(SCM sexp, SCM port, int writing);
extern int scm_bigprint(SCM exp, SCM port, int writing);
extern SCM scm_istr2int(char *str, long len, long radix);
extern SCM scm_istr2int(char *str, long len, long radix);
extern SCM scm_istr2flo(char *str, long len, long radix);
extern SCM scm_istring2number(char *str, long len, long radix);
extern SCM scm_string_to_number(SCM str, SCM radix);
extern SCM scm_makdbl (double x, double y);
extern SCM scm_bigequal(SCM x, SCM y);
extern SCM scm_floequal(SCM x, SCM y);
extern SCM scm_number_p(SCM x);
extern SCM scm_real_p(SCM x);
extern SCM scm_int_p(SCM x);
extern SCM scm_inexact_p(SCM x);
extern SCM scm_num_eq_p (SCM x, SCM y);
extern SCM scm_lt_p(SCM x, SCM y);
extern SCM scm_gt_p(SCM x, SCM y);
extern SCM scm_le_p(SCM x, SCM y);
extern SCM scm_ge_p(SCM x, SCM y);
extern SCM scm_zero_p(SCM z);
extern SCM scm_positive_p(SCM x);
extern SCM scm_negative_p(SCM x);
extern SCM scm_max(SCM x, SCM y);
extern SCM scm_min(SCM x, SCM y);
extern SCM scm_sum(SCM x, SCM y);
extern SCM scm_difference(SCM x, SCM y);
extern SCM scm_product(SCM x, SCM y);
extern double scm_num2dbl (SCM a, SCM msg, SCM why);
extern SCM scm_divide(SCM x, SCM y);
extern double scm_asinh(double x);
extern double scm_acosh(double x);
extern double scm_atanh(double x);
extern double scm_truncate(double x);
extern double scm_round(double x);
extern double scm_exact_to_inexact(double z);
extern void scm_two_doubles(SCM z1, SCM z2, SCM sstring, struct scm_dpair *xy);
extern SCM scm_expt(SCM z1, SCM z2);
extern SCM scm_atan2(SCM z1, SCM z2);
extern SCM scm_make_rectangular(SCM z1, SCM z2);
extern SCM scm_make_polar(SCM z1, SCM z2);
extern SCM scm_real_part(SCM z);
extern SCM scm_imag_part(SCM z);
extern SCM scm_angle(SCM z);
extern SCM scm_inexact_to_exact(SCM z);
extern SCM scm_trunc(SCM x);
extern SCM scm_dbl2big(double d);
extern double scm_big2dbl(SCM b);
extern SCM scm_long2num(long sl);
extern SCM scm_ulong2num(unsigned long sl);
extern long scm_num2long(SCM num, SCM pos, SCM s_caller);
extern long num2long(SCM num, SCM pos, SCM s_caller);
extern unsigned long scm_num2ulong_maybe (int * okp, SCM num);
extern unsigned long scm_num2ulong (SCM num, SCM msg, SCM s_caller);
extern void scm_init_numbers (void);
#endif  /* INCLUDE__LIBSYSTAS__NUMBERS_H */
