/* tag: Tom Lord Tue Dec  4 14:41:43 2001 (unicode.c)
 */
/* unicode.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/unicode/unicode.h"



/************************************************************************
 *(h1 "Representing Unicode Strings")
 * 

The Hackerlab C Library is designed to operate correctly for programs
which internally use any ^combination^ of the encodings iso8859-1,
utf-8, and utf-16.  (Future releases are likely to add support for
utf-32.)

*/

/*(c uni_string :category type)
 * typedef struct uni__undefined_struct * uni_string;
 * 
 * The type `uni_string' is pointer to a value of unknown size.  It is
 * used to represent the address of a Unicode string or an address
 * within a Unicode string.
 * 
 * Any two `uni_string' pointers may be compared for equality.
 * 
 * `uni_string' pointers within a single string may be compared
 * using any relational operator (`<', `>', etc.).
 * 
 * `uni_string' pointers are created from UTF-8 pointers (`t_uchar *')
 * and from UTF-16 pointers (`t_unichar *') by means of a cast:
 * 
 * 	uni_string s = (uni_string)utf_8_string;
 * 	uni_string t = (uni_string)utf_16_string;
 * 
 * By convention, all functions that operate on Unicode strings
 * accept two parameters for each string: an encoding form, and
 * a string pointer as in this function declaration:
 * 
 *	void uni_fn (enum uni_encoding_scheme encoding,
 *		     uni_string s);
 * 
 * By convention, the length of a Unicode string is always measured
 * in code units, no matter what the size of those code units.
 * Integer string indexes are also measured in code units.
 */


/************************************************************************
 *(h2 "Basic Unicode String Functions")
 * 
 * 
 * These functions were not ready for the current release of the
 * Hackerlab C Library.  They will be included in future releases.
 */
