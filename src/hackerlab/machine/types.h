/* types.h  - basic machine type declarations
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__MACHINE__TYPES_H
#define INCLUDE__MACHINE__TYPES_H

#include "hackerlab/os/stddef.h"
#include "hackerlab/os/limits.h"
#include "hackerlab/os/sys/types.h"
#include "hackerlab/machine/alignment.h"



/***************************************************************** 
 * t_uint8, t_int8
 *
 */

#if UCHAR_MAX == 0xff
typedef unsigned char t_uint8;
typedef char t_int8;
#else
#error "Characters aren't 8 bits?"
#endif


/***************************************************************** 
 * t_uint16, t_int16
 *
 */

#if USHRT_MAX == 0xffff
typedef unsigned short t_uint16;
typedef short t_int16;
#elif UINT_MAX == 0xffff
typedef unsigned int t_uint16;
typedef int t_int16;
#elif ULONG_MAX == 0xffff
typedef unsigned long t_uint16;
typedef long t_int16;
#else
#error "No 16 bit integer type?"
#endif


/***************************************************************** 
 * t_uint32, t_int32
 *
 */

#if USHRT_MAX == 0xffffffff
typedef unsigned short t_uint32;
typedef short t_int32;
#elif UINT_MAX == 0xffffffff
typedef unsigned int t_uint32;
typedef int t_int32;
#elif ULONG_MAX == 0xffffffff
typedef unsigned long t_uint32;
typedef long t_int32;
#else
#error "No 32 bit integer type?"
#endif


/****************************************************************
 * `t_' shorthands for unsigned integer types
 *
 */

typedef unsigned char t_uchar;
typedef unsigned short  t_ushort;
typedef unsigned int  t_uint;
typedef unsigned long t_ulong;


/****************************************************************
 * sizeof macros
 */

#if USHRT_MAX == 0xff
#define MACHINE_SIZEOF_SHORT		(1)
#elif USHRT_MAX == 0xffff
#define MACHINE_SIZEOF_SHORT		(2)
#elif USHRT_MAX == 0xffffffff	
#define MACHINE_SIZEOF_SHORT		(4)
#elif USHRT_MAX == ((0xffffffff << 32) + 0xffffffff)
#define MACHINE_SIZEOF_SHORT		(8)
#else
#error "weird sizeof(short)"
#endif


#if UINT_MAX == 0xff
#define MACHINE_SIZEOF_INT		(1)
#elif UINT_MAX == 0xffff
#define MACHINE_SIZEOF_INT		(2)
#elif UINT_MAX == 0xffffffff	
#define MACHINE_SIZEOF_INT		(4)
#elif UINT_MAX == ((0xffffffff << 32) + 0xffffffff)
#define MACHINE_SIZEOF_INT		(8)
#else
#error "weird sizeof(short)"
#endif


#if ULONG_MAX == 0xff
#define MACHINE_SIZEOF_LONG		(1)
#elif ULONG_MAX == 0xffff
#define MACHINE_SIZEOF_LONG		(2)
#elif ULONG_MAX == 0xffffffff	
#define MACHINE_SIZEOF_LONG		(4)
#elif ULONG_MAX == ((0xffffffff << 32) + 0xffffffff)
#define MACHINE_SIZEOF_LONG		(8)
#else
#error "weird sizeof(short)"
#endif



/***************************************************************** 
 * unichar
 *
 */

/* A 16-bit character:
 */
typedef t_uint16 t_unichar;

/* A unicode scalar value
 */
typedef t_uint32 t_unicode;



/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__MACHINE__TYPES_H */
