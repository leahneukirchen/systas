/* tag: Tom Lord Tue Dec  4 14:41:45 2001 (unidata.h)
 */
/* unidata.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNIDATA__UNIDATA_H
#define INCLUDE__UNIDATA__UNIDATA_H


#include "hackerlab/uni/unidata.h"
#include "hackerlab/unidata/bitsets.h"
#include "hackerlab/unidata/bitset-lookup.h"
#include "hackerlab/unidata/blocks.h"
#include "hackerlab/unidata/db.h"
#include "hackerlab/unidata/db-macros.h"
#include "hackerlab/unidata/case-db.h"
#include "hackerlab/unidata/case-db-macros.h"
#include "hackerlab/unidata/combine-db.h"
#include "hackerlab/unidata/combine-db-macros.h"
#include "hackerlab/unidata/decomp-db.h"
#include "hackerlab/unidata/decomp-db-macros.h"




#define unidata__db_data(CODE)  (unidata__db_ref (unidata__db, CODE))

#if defined(__GNUC__) && defined(UNIDATA_INLINES)

#undef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS static inline

#include "hackerlab/unidata/db-inlines.c"
#include "hackerlab/unidata/case-db-inlines.c"

#else

#undef UNI_INLINE_QUALIFIERS

#include "hackerlab/unidata/db-inlines.h"
#include "hackerlab/unidata/case-db-inlines.h"

#endif


/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNIDATA__UNIDATA_H */
