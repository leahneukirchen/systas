/* escape.h - Non-local exits from long-running Rx functions.
 *
 ****************************************************************
 * Copyright (C) 1999, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX__ESCAPE_H
#define INCLUDE__RX__ESCAPE_H


#include "hackerlab/os/setjmp.h"



extern jmp_buf rx_escape_jmp_buf;
extern void (*rx_poll)(void);


/* automatically generated __STDC__ prototypes */
extern void rx_escape (void);
#endif  /* INCLUDE__RX__ESCAPE_H */
