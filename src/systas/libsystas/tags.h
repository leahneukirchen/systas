/* tags.h - Tag bits and the representation of scheme objects.
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#ifndef INCLUDE__LIBSYSTAS__TAGS_H
#define INCLUDE__LIBSYSTAS__TAGS_H


/************************************************************************
 *(h1 "In the Beginning was the Word")
 * 
 * In C, all Scheme values are passed as parameters, returned from
 * functions, and referenced in data structures as word-sized values
 * of type `SCM'.
 * 
 insert*/

typedef long SCM;

/*end-insert
 *
 * Programmers should regard values of type `SCM' as nearly opaque:
 * they are accessed and modified only through macros and functions.
 * The only operation that programs can perform on these values
 * directly is a test for equality: SCM values are equal in Scheme (in
 * the sense of `eq?') if and only if they are equal in C (in the
 * sense of `==').
 * 
 * There are three categories of values of type SCM: "immediates",
 * "non-immediates", and "non-objects".
 *
 * *Immediates* -- values representing a Scheme object that fits
 * entirely within one word.  Such objects are not subject to mutation
 * or garbage collection.  Characters and small integers are examples
 * of immediate values.  Immediate values have a type tag in two or
 * three low order bits of the value, and type-specific information in
 * the remaining bits.
 *
 * *Non-immediates* -- values representing a Scheme object stored in
 * memory allocated from the garbage collected portion of the heap.
 * Cons pairs and vectors are examples of objects passed as
 * non-immediate values.
 * 
 * Non-immediate values point to a two-word structure of type `struct
 * scm_cell':
 insert*/

struct scm_cell
{
  SCM car;
  SCM cdr;
};

/*end-insert
 * 
 * If the value refers to a "cons pair" (see *XREF*:), the two fields
 * are the `car' and `cdr' of the pair.  If the value refers to some
 * other type, the `car' field of the cell holds a type tag (and
 * possibly other data), and the `cdr' holds type-specific
 * information.
 *
 * `scm_cell' structures are always aligned on two-word boundries.
 * Consequently, the three low-order bits of a pointer to an
 * `scm_cell' are always 0.
 * 
 *
 * *Non-objects* -- meaning that the 
 * 
 * Immediates and non-immediates are distinguished by bits two and
 * three.  Immediate values must have a 1 in at least one of those
 * bits.
 *
 */

/*(c SCM_IS_IMMEDIATE)
 * int SCM_IS_IMMEDIATE (SCM obj);
 * 
 * Return a non-0 value if `obj' is an immediate object
 */
#define SCM_IS_IMMEDIATE(x) 		(6 & (int)(x))



/* {Immediate Values} 
 */

#define scm_tc8_char		0xf4
#define scm_tc8_iloc		0xfc

#define SCM_ITAG8(X)		((int)(X) & 0xff)
#define SCM_MAKE_ITAG8(X, TAG)	(((X)<<8) + TAG)
#define SCM_ITAG8_DATA(X)	((X)>>8)



/* Immediate Symbols, Special Symbols, Flags (various constants).
 */

/* SCM_ISYMP tests for ISPCSYM and ISYM */
#define SCM_ISYMP(n) 		((0x187 & (int)(n))==4)

/* SCM_IFLAGP tests for ISPCSYM, ISYM and IFLAG */
#define SCM_IFLAGP(n) 		((0x87 & (int)(n))==4)
#define SCM_ISYMNUM(n) 		((int)((n)>>9))
#define SCM_ISYM_CHARS(n) 	(scm_isymnames[SCM_ISYMNUM(n)])
#define SCM_MAKSPCSYM(n) 	(((n)<<9)+((n)<<3)+4L)
#define SCM_MAKISYM(n) 		(((n)<<9)+0x74L)
#define SCM_MAKIFLAG(n) 	(((n)<<9)+0x174L)

/* These are immediate values of special significance to EVAL.
 *
 * These are used only in eval but their values
 * have to be allocated here.
 *
 */
#define SCM_SPECIAL_IMMEDIATES \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_AND, SCM_MAKSPCSYM(0), "#@and"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_BEGIN, SCM_MAKSPCSYM(1), "#@begin"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_CASE, SCM_MAKSPCSYM(2), "#@case"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_COND, SCM_MAKSPCSYM(3), "#@cond"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_DO, SCM_MAKSPCSYM(4), "#@do"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_IF, SCM_MAKSPCSYM(5), "#@if"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_LAMBDA, SCM_MAKSPCSYM(6), "#@lambda"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_LET, SCM_MAKSPCSYM(7), "#@let"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_LETSTAR, SCM_MAKSPCSYM(8), "#@let*"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_LETREC, SCM_MAKSPCSYM(9), "#@letrec"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_OR, SCM_MAKSPCSYM(10), "#@or"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_QUOTE, SCM_MAKSPCSYM(11), "#@quote"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_SET, SCM_MAKSPCSYM(12), "#@set!"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_DEFINE, SCM_MAKSPCSYM(13), "#@define"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_APPLY, SCM_MAKISYM(14), "#@apply"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_EVAL, SCM_MAKISYM(15), "#@eval"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_EVAL2, SCM_MAKISYM(16), "#@eval2"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_EVAL_X, SCM_MAKISYM(17), "#@eval!"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_EVAL2_X, SCM_MAKISYM(18), "#@eval2!"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_THE_ENV, SCM_MAKISYM(19), "#@the-environment"), \
	SCM_SPECIAL_IMMEDIATE (SCM_IM_CONT, SCM_MAKISYM(20), "#@call-with-current-continuation"), \
	SCM_SPECIAL_IMMEDIATE (SCM_WILL_BE_BOOL_F, SCM_MAKIFLAG(21), "#f"), \
	SCM_SPECIAL_IMMEDIATE (SCM_BOOL_T, SCM_MAKIFLAG(22), "#t"), \
	SCM_SPECIAL_IMMEDIATE (SCM_UNDEFINED, SCM_MAKIFLAG(23), "#<undefined>"), \
	SCM_SPECIAL_IMMEDIATE (SCM_EOF_VAL, SCM_MAKIFLAG(24), "#<eof>"), \
	SCM_SPECIAL_IMMEDIATE (SCM_EOL, SCM_MAKIFLAG(25), "()"), \
	SCM_SPECIAL_IMMEDIATE (SCM_UNSPECIFIED, SCM_MAKIFLAG(26), "#<unspecified>")

#define SCM_BOOL_F SCM_EOL

enum scm_special_immediates
{
#undef SCM_SPECIAL_IMMEDIATE
#define SCM_SPECIAL_IMMEDIATE(NAME,VALUE,PRINTNAME)	NAME = VALUE
  SCM_SPECIAL_IMMEDIATES,
};

#define SCM_UNBNDP(x) 	(SCM_UNDEFINED==(x))


/* {Non-immediate values.}
 *
 * If X is non-immediate, it is necessary to look at SCM_CAR (X) to
 * figure out Xs type.   X may be a cons pair, in which case the
 * value SCM_CAR (x) will be either an immediate or non-immediate value.
 * X may be something other than a cons pair, in which case the value SCM_CAR (x)
 * will be a non-object value.  
 *
 * All immediates and non-immediates have a 0 in bit 0.  All non-object values
 * have a 1 in bit 0.
 */

/* Pointer into the heap?
 */
#define SCM_CELLP(x) 	(!SCM_NCELLP(x))
#define SCM_NCELLP(x) 	((sizeof(struct scm_cell)-1) & (int)(x))

/* Does the heap pointer point to a cons pair?
 */
#define SCM_NCONSP(x) (1 & (int)SCM_CAR(x))
#define SCM_CONSP(x) (!SCM_NCONSP(x))


/* Does the heap pointer point to a cons pair or gloc?
 */
#define SCM_ECONSP(x) (SCM_CONSP (x) || (SCM_TYP3(x) == 1))
#define SCM_NECONSP(x) (SCM_NCONSP(x) && (SCM_TYP3(x) != 1))


/* See libsystas/numbers.h for macros relating to immediate integers. 
 */

#define SCM_ITAG3(x) 		(7 & (int)x)
#define SCM_TYP3(x) 		SCM_ITAG3 (SCM_CAR(x))

#define scm_tc3_cons		0
#define scm_tc3_cons_gloc	1
#define scm_tc3_int_1		2
#define scm_tc3_closure		3
#define scm_tc3_imm24		4
#define scm_tc3_tc7_1		5
#define scm_tc3_int_2		6
#define scm_tc3_tc7_2		7

#define SCM_R_BITS		0x7d
#define SCM_U_BITS		0x77
#define SCM_S_BITS		0x6f
#define SCM_SU_BITS		(SCM_S_BITS & SCM_U_BITS)
#define SCM_RS_BITS		(SCM_R_BITS & SCM_S_BITS)
#define SCM_RU_BITS		(SCM_R_BITS & SCM_U_BITS)
#define SCM_RSU_BITS		(SCM_R_BITS & SCM_S_BITS & SCM_U_BITS)

#define SCM_TYP7(x) 		(0x7f & (int)SCM_CAR(x))
#define SCM_TYP7S(x) 		(SCM_S_BITS & (int)SCM_CAR(x))
#define SCM_TYP7U(x) 		(SCM_U_BITS & (int)SCM_CAR(x))
#define SCM_TYP7SU(x) 		(SCM_SU_BITS & (int)SCM_CAR(x))
#define SCM_TYP7R(x) 		(SCM_R_BITS & (int)SCM_CAR(x))
#define SCM_TYP7RS(x) 		(SCM_RS_BITS & (int)SCM_CAR(x))
#define SCM_TYP7RU(x) 		(SCM_RU_BITS & (int)SCM_CAR(x))
#define SCM_TYP7RSU(x) 		(SCM_RSU_BITS & (int)SCM_CAR(x))

#define SCM_TYP16(x) 		(0xff7f & (int)SCM_CAR(x))
#define SCM_TYP16S(x) 		(0xfe7f & (int)SCM_CAR(x))



/* The arrangement of many of these tag values
 * in sequence is critical.
 */

/* For strings:
 *
 * R = 0	-- read-only string (symbol or shared subsymbol)
 * R = 1	-- read-write string
 *
 * S = 0	-- character data allocated by malloc
 * S = 1	-- character data not allocated by malloc
 *
 * U = 0	-- complete string
 * U = 1	-- shared substring
 *
 */

#define scm_tc7_R_flags		0x2
#define scm_tc7_S_flag		0x10
#define scm_tc7_U_flag		0x08

#define scm_tc7_writable_flag		scm_tc7_R_flags
#define scm_tc7_static_allocation_flag	scm_tc7_S_flag
#define scm_tc7_shared_substring_flag	scm_tc7_U_flag

#define scm_tc7_base_string_tag		5

#define scm_tc7_symbol			scm_tc7_base_string_tag
#define scm_tc7_string			(scm_tc7_base_string_tag | scm_tc7_writable_flag)
#define scm_tc7_subsymbol		(scm_tc7_base_string_tag | scm_tc7_shared_substring_flag)
#define scm_tc7_substring		(scm_tc7_base_string_tag | scm_tc7_shared_substring_flag | scm_tc7_writable_flag)

#define scm_tc7_static_symbol		(scm_tc7_base_string_tag | scm_tc7_static_allocation_flag)
#define scm_tc7_static_string		(scm_tc7_base_string_tag | scm_tc7_writable_flag | scm_tc7_static_allocation_flag)
#define scm_tc7_static_subsymbol	(scm_tc7_base_string_tag | scm_tc7_shared_substring_flag | scm_tc7_static_allocation_flag)
#define scm_tc7_static_substring	(scm_tc7_base_string_tag | scm_tc7_shared_substring_flag | scm_tc7_writable_flag | scm_tc7_static_allocation_flag)

/* Unused:
 *						 * xx S U x R 1 *
 *						 * ============ *
 * #define scm_tc7_xxxxx  	37		 * 01 0 0 1 0 1 *
 * #define scm_tc7_xxxxx 	39		 * 01 0 0 1 1 1 *
 * #define scm_tc7_xxxxx     	45		 * 01 0 1 1 0 1 *
 * #define scm_tc7_xxxxx   	47		 * 01 0 1 1 1 1 *
 */

/* Unused:
 *						 * xx S U x R 1 *
 *						 * ============ *
 * #define scm_tc7_xxxxx 	53		   01 1 0 1 0 1
 * #define scm_tc7_xxxxx	55		   01 1 0 1 1 1
 * #define scm_tc7_xxxxx     	61		   01 1 1 1 0 1
 * #define scm_tc7_xxxxx   	63		   01 1 1 1 1 1
 */

						/* xx S U x R 1 */
						/* ============ */
#define scm_tc7_vector		69		/* 10 0 0 1 0 1 */
#define scm_tc7_wvect		71		/* 10 0 0 1 1 1 */
/* Unused:
 * #define scm_tc7_xxxxx	77		 * 10 0 1 1 0 1 *
 * #define scm_tc7_xxxxx        79		 * 10 0 1 1 1 1 *
 */

/* These tc7_ codes don't make any particular use of the 
 * Q, D, and S option bits.
 */
#define scm_tc7_contin		85		/* continuation */
#define scm_tc7_cclo		87		/* compiled closure */
#define scm_tc7_rpsubr		93		/* relational predicate (>, <, etc) */
#define scm_tc7_cxr		95		/* car, cdr, caar, cadr, etc. 
						 *
						 * Also, procedures which take one
						 * "double" argument and return a "double"
						 */
#define scm_tc7_subr_3		101		/* 3 argument builtin */
#define scm_tc7_subr_2		103		/* 2 argument builtin */
#define scm_tc7_asubr		109		/* associative builtin */
#define scm_tc7_subr_1o		111		/* 1 optional argument builtin */
#define scm_tc7_lsubr_2		117		/* 2 or more arguments builtin */
#define scm_tc7_lsubr		119		/* 0 or more arguments builtin */


#define scm_tc7_unused		125

/* fports and pipes form an intended TYP16S equivelancy
 * group.
 */
#define scm_tc16_fport 		(scm_tc7_port + 0*256L)
#define scm_tc16_pipe 		(scm_tc7_port + 1*256L)

#define scm_tc16_strport	(scm_tc7_port + 2*256L)
#define scm_tc16_sfport 	(scm_tc7_port + 3*256L)


/* There are 256 smob subtypes.  Here are the first four.
 */

#define scm_tc7_smob		127

/* scm_tc_free_cell is also the 0th smob type.
 */
#define scm_tc_free_cell	scm_tc7_smob

/* The 1st smob type:
 */
#define scm_tc16_flo		0x017f

/* Some option bits begeinning at bit 16 of scm_tc16_flo:
 */
#define SCM_REAL_PART		(1L<<16)
#define SCM_IMAG_PART		(2L<<16)
#define scm_tc_dblr		(scm_tc16_flo|SCM_REAL_PART)
#define scm_tc_dblc		(scm_tc16_flo|SCM_REAL_PART|SCM_IMAG_PART)

/* Smob types 2 and 3: 
 */
#define scm_tc16_bigpos		0x027f
#define scm_tc16_bigneg		0x037f



/* Is non-immediate `x' a symbol? 
 *
 */
#define SCM_SYMBOLP(x) 				(SCM_TYP7S(x) == scm_tc7_symbol)



/****************************************************************
 * Length and Data Macros
 *
 * Many types use the following macros including:
 *
 * 	all kinds of strings
 * 	all kinds of vectors
 *	compiled closures
 *	continuations
 *
 * All types that use these macros have a 7-bit type-code and 24-bit
 * "length" in the car of a non-immediate object, and a 32-bit data 
 * pointer in the cdr.
 */

/* How many bytes in the string (any kind of string)?
 */ 
#define SCM_LENGTH(x) 				(((unsigned long)SCM_CAR(x))>>8)

/* What is the length of the longest permitted string?
 */
#define SCM_LENGTH_MAX 				(0xffffffL)

/* Set the length and 7-bit type-tag of a non-immediate object.
 */
#define SCM_SET_LENGTH(x, v, t) 			(SCM_CAR(x) = (((v)<<8)+(t)))


/* Return the data pointer of a non-immediate object.
 */
#define SCM_CHARS(x) 				((char *)(SCM_CDR(x)))
#define SCM_UCHARS(x) 				((t_uchar *)(SCM_CDR(x)))


/****************************************************************
 * SCM_RO_ -- read only strings.
 * 
 * Read-only strings should not be mutated and are not necessarily 
 * 0-terminated.
 *
 * Regular strings are read-only strings.  So are symbols and 
 * substrings.
 */


/* Length and data macros for read-only strings:
 */
#define SCM_RO_CHARS(x) 	((SCM_TYP7RS(x) == scm_tc7_subsymbol) \
				 ? SCM_INUM (SCM_CADR (x)) + SCM_CHARS (SCM_CDDR (x))  \
				 : SCM_CHARS (x))
#define SCM_RO_UCHARS(x) 	((SCM_TYP7RS(x) == scm_tc7_subsymbol) \
				 ? SCM_INUM (SCM_CADR (x)) + SCM_UCHARS (SCM_CDDR (x))  \
				 : SCM_UCHARS (x))
#define SCM_RO_LENGTH(x) 	SCM_LENGTH (x)



/****************************************************************
 * SCM_STRING_ -- basic strings and substrings of basic strings.
 * 		  (aka writable strings)
 *
 * Writable strings may be mutated and are not necessarily 
 * 0-terminated.
 */


/* Length and data macros for writable strings:
 */
#define SCM_STRING_CHARS(x)	SCM_RO_CHARS(x)
#define SCM_STRING_UCHARS(x)	SCM_RO_UCHARS(x)
#define SCM_STRING_LENGTH(x)    SCM_RO_LENGTH(x)



/****************************************************************
 * SCM_SUBSTR -- substrings
 *
 * Substrings share their data with some other string which is
 * not a substring.
 */


/* Return the string with which substring X shares data:
 */
#define SCM_SUBSTR_STR(x) 	(SCM_CDDR (x))

/* If X shares data with string S, return the offset within S
 * of the beginning of X's data.
 */
#define SCM_SUBSTR_OFFSET(x) 	(SCM_CADR (x))



/* Dispatching aids:
 *
 * These macros make it easier to write switch statements
 * that dispatch on the types of objects.
 */

/* scm_tcs_cons_imcar
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_cons_imcar:
 *		`obj' is a cons pair with an immediate value in the CAR.
 *     ...
 *   }
 */
#define scm_tcs_cons_imcar 2:case 4:case 6:case 10:\
 case 12:case 14:case 18:case 20:\
 case 22:case 26:case 28:case 30:\
 case 34:case 36:case 38:case 42:\
 case 44:case 46:case 50:case 52:\
 case 54:case 58:case 60:case 62:\
 case 66:case 68:case 70:case 74:\
 case 76:case 78:case 82:case 84:\
 case 86:case 90:case 92:case 94:\
 case 98:case 100:case 102:case 106:\
 case 108:case 110:case 114:case 116:\
 case 118:case 122:case 124:case 126

/* scm_tcs_cons_nimcar
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_cons_nimcar:
 *		`obj' is a cons pair with an non-immediate value in the CAR.
 *     ...
 *   }
 */
#define scm_tcs_cons_nimcar 0:case 8:case 16:case 24:\
 case 32:case 40:case 48:case 56:\
 case 64:case 72:case 80:case 88:\
 case 96:case 104:case 112:case 120


/* scm_tcs_cons_gloc
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_cons_gloc:
 *		`obj' is a cons pair with a gloc in the CAR.
 *		A gloc is a memoized reference to
 *		a top-level variable. See "eval.h".
 *     ...
 *   }
 */
#define scm_tcs_cons_gloc 1:case 9:case 17:case 25:\
 case 33:case 41:case 49:case 57:\
 case 65:case 73:case 81:case 89:\
 case 97:case 105:case 113:case 121

/* scm_tcs_cons_gloc
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_cons_closures:
 *		`obj' is a closure.
 *     ...
 *   }
 */
#define scm_tcs_closures   3:case 11:case 19:case 27:\
 case 35:case 43:case 51:case 59:\
 case 67:case 75:case 83:case 91:\
 case 99:case 107:case 115:case 123

/* scm_tcs_subrs
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_subrs:
 *		`obj' is a built-in procedure.
 *     ...
 *   }
 */
#define scm_tcs_subrs scm_tc7_asubr:case scm_tc7_cxr:\
 case scm_tc7_subr_3:case scm_tc7_subr_2:case scm_tc7_rpsubr:case scm_tc7_subr_1o:\
 case scm_tc7_lsubr_2:case scm_tc7_lsubr

/* scm_tcs_symbols
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_symbols:
 *		`obj' is a symbol.
 *     ...
 *   }
 */
#define scm_tcs_symbols scm_tc7_symbol:case scm_tc7_static_symbol

/* scm_tcs_bignums
 *
 * switch (SCM_TYP7(obj))
 *   {
 *     case scm_tcs_bignums:
 *		`obj' is a bignum.
 *     ...
 *   }
 */
#define scm_tcs_bignums scm_tc16_bigpos:case scm_tc16_bigneg

/* There should be more of these (for strings).
 */

/* Here is a summary of tagging in SCM values as they might occur in
 * SCM variables or in the heap. 
 *
 * low bits    meaning
 *
 * 
 * 0		Most objects except...
 * 1 		...glocs (this tag valid only in a CAR)
 *			bit 0 is reserved as a GC mark bit.
 *			a gloc can only occur in the CAR of a pair,
 *			so its bit-0 is available.
 *
 * 00		heap addresses (e.g. pairs) and many immediates (not integers)
 * 01		glocs, some tc7_ codes
 * 10		immediate integers
 * 11		various tc7_ codes including, tc16_ codes.
 *
 *
 * 000		heap address
 * 001		glocs
 * 010		integer
 * 011		closure
 * 100		immediates
 * 101		tc7_
 * 110 		integer
 * 111		tc7_
 *
 *
 * 100 --- IMMEDIATES
 *
 * Looking at the seven final bits of an immediate:
 * 
 * 0000-100	short instruction
 * 0001-100	short instruction
 * 0010-100	short instruction
 * 0011-100	short instruction
 * 0100-100	short instruction
 * 0101-100	short instruction
 * 0110-100	various immediates and long instructions
 * 0111-100	short instruction
 * 1000-100	short instruction
 * 1001-100	short instruction
 * 1010-100	short instruction
 * 1011-100	short instruction
 * 1100-100	short instruction
 * 1101-100	short instruction
 * 1110-100	immediate characters
 * 1111-100	ilocs
 *
 * Some of the 0110100 immediates are long instructions (they dispatch 
 * in two steps compared to one step for a short instruction).
 * The two steps are, (1) dispatch on 7 bits to the long instruction
 * handler, (2) dispatch on 7 additional bits.
 *
 * One way to think of it is that there are 128 short instructions,
 * with the 13 immediates above being some of the most interesting.
 *
 * Also noteworthy are the groups of 16 7-bit instructions implied by
 * some of the 3-bit tags.   For example, closure references consist
 * of an 8-bit aligned address tagged with 011.  There are 16 corresponding
 *  7-bit instructions, all ending 011, which are invoked by evaluating 
 * closures.
 *
 * In other words, if you hand the evaluator a closure, the evaluator
 * treats the closure as a graph of virtual machine instructions.
 * A closure is a pair with a pointer to the body of the procedure
 * in the CDR and a pointer to the environment of the closure in the CAR.
 * The environment pointer is tagged 011 which implies that the least
 * significant 7 bits of the environment pointer also happen to be
 * a virtual machine instruction we could call "SELF" (for self-evaluating
 * object).
 *
 * A less trivial example are the 16 instructions ending 000.  If those
 * bits tag the CAR of a pair, then evidently the pair is an ordinary
 * cons pair and should be evaluated as a procedure application.  The sixteen,
 * 7-bit 000 instructions are all "NORMAL-APPLY"  (Things get trickier.
 * For example, if the CAR of a procedure application is a symbol, the NORMAL-APPLY
 * instruction will, as a side effect, overwrite that CAR with a new instruction
 * that contains a cached address for the variable named by the symbol.)
 *
 * Here is a summary of tags in the CAR of a non-immediate:
 *
 *   HEAP CELL:	G=gc_mark; 1 during mark, 0 other times.
 *
 * cons	   ..........SCM car..............0  ...........SCM cdr.............G
 * gloc    ..........SCM vcell..........001  ...........SCM cdr.............G
 * closure ..........SCM code...........011  ...........SCM env.............G
 * tc7	   .........long length....GRxxU1S1  ..........void *data............
 *
 * 101 & 111 --- tc7_ types
 *
 *		tc7_tags are 7 bit tags ending in 1.  These tags occur
 *		only in the CAR of heap cells.
 *
 *		SCM_LENGTH returns the bits in "length" (see the diagram).
 *		SCM_CHARS returns the data cast to "char *"
 *		SCM_CDR returns the data cast to "SCM"
 *		TYP7(X) returns bits 0...6 of SCM_CAR (X)
 *
 *		For the interpretation of SCM_LENGTH and SCM_CHARS
 *		that applies to a particular type, see the header file
 *		for that type.
 *
 *		TYP7S(X) returns TYP7, but masking out the option bit S.
 *		TYP7U(X) returns TYP7, but masking out the option bit U.
 *		TYP7R(X) returns TYP7, but masking out the option bit R.
 *		TYP7US(X), TYP7RS, TYP7RU mask out two option bits.
 *		TYP7RUS(X) masks out all three option bits.
 *
 *		For an example:
 *					     	     RU S
 *		  scm_tc7_symbol    		= Gxx00101
 *		  scm_tc7_string 		= Gxx00111
 *
 *		TYP7S turns tc7_string into tc7_symbol.
 *
 *		Some TC7 types are subdivided into 256 subtypes giving
 *		rise to the macros:
 *
 *		TYP16
 *		TYP16S
 *
 *		TYP16S functions similarly wrt to TYP16 as TYP7S to TYP7,
 *		but a different option bit is used (bit 2 for TYP7S,
 *		bit 8 for TYP16S).
 *
 */
#endif  /* INCLUDE__LIBSYSTAS__TAGS_H */
