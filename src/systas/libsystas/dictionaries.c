/* tag: Tom Lord Tue Dec  4 14:41:52 2001 (dictionaries.c)
 */
/* dictionaries.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include "systas/libsystas/dictionaries.h"


/****************************************************************
 *(h0 "Rationale -- Dictionaries in General")
 *
 * A "dictionary" is defined to be any data structure that maintains
 * an association between "keys" and "values".  A wide variety of
 * implementations and semantics are possible for dictionaries.  Some
 * examples are association lists, keyword-argument lists, hash
 * tables, weak hash tables, and so on.
 * 
 * Systas Scheme does not contain a universal "dictionary type" of which
 * all actual dictionaries are instances and it does not contain a set of
 * generic dictionary procedures that can operate on any dictionary,
 * regardless of its actual type.  Instead, Systas defines some common
 * minimal calling conventions that apply to all dictionaries.  Because all
 * dictionaries have these calling conventions in common, code that you
 * write has great flexibility in how it mixes and matches dictionary
 * representations.
 * 
 * There are three basic dictionary operators:
 * 
 * 	;; Looking Up Values
 * 	;; =================
 * 	;;
 * 	;; Return the value bound to `key' in `dictionary'.  
 * 	;; Return #f if `key' has no binding.
 * 	;;
 * 	(<dictionary-type>-ref dictionary key)
 * 
 * 
 * 	;; Defining Values
 * 	;; ===============
 * 	;;
 * 	;; Set the value bound to `key' in `dictionary'.
  	;; 
 * 	(<dictionary-type>-set! dictionary key value)
 * 	
 * 	;; This may construct a new head for the dictionary.  
 * 	;; If your dictionary is bound to `x', be sure to use:
 * 	;;
 * 	(set! x (<dictionary-type>-set! x key value))
 * 	;;
 * 	;; NOT:  (<dictionary-type>-set! x key value)
 * 
 * 
 * 	;; Removing Values
 * 	;; ===============
 * 	;; Remove `key' from `dictionary'.
 * 	;;
 * 	(<dictionary-type>-remove! dictionary key)
 *      
 * 	;; This may construct a new head for the dictionary.  
 * 	;; If your dictionary is bound to `x', be sure to use:
 * 	;;
 * 	(set! x (<dictionary-type>-remove! x key value))
 * 	;;
 * 	;; NOT:  (<dictionary-type>-remove! x key value)
 * 
 * Here is one way to use these conventions.  Suppose that your code
 * involves a dictionary-style data structure.  Initially you choose to
 * use association lists.  You might write:
 * 
 *   (define (make-widget-dictionary)   	'())
 *   (define (widget-dictionary-ref . args)	(apply assq-ref args))
 *   (define (widget-dictionary-set! . args)    (apply assq-set! args))
 *   (define (widget-dictionary-remove! . args) (apply assq-remove! args))
 * 
 * Later, you can change to hash tables by changing just those four
 * lines:
 * 
 *   (define (make-widget-dictionary)   	(make-hash-table))
 *   (define (widget-dictionary-ref . args)	(apply hashq-ref args))
 *   (define (widget-dictionary-set! . args)    (apply hashq-set! args))
 *   (define (widget-dictionary-remove! . args) (apply hashq-remove! args))
 * 
 * It is similarly straightforward to define ways to choose the
 * representation of a dictionary on-the-fly, or even to change the
 * representation of a dictionary in a running program.
 * 
 * Another aspect of the dictionary calling convention is that for each
 * type of dictionary data structure, three dictionary types are usually
 * provided: one each for the possibilities of comparing keys using
 * `eq?', `eqv?', and `equal?'.  We try to make it so that `eq?' and
 * `eqv?'  dictionary procedures are named with abbreviations using "q"
 * and "v", while the `equal?' version uses no suffix character.  This
 * convention gives the cleanest looking names to "equal?" based
 * procedures: `hash-ref', `assoc-ref'.
 * 
 * Finally, some dictionary types can be extended beyond `eq?', `eqv?'
 * and `equal?' to be based on an arbitrary equivalence relation.  When
 * this is the case, a generalized form of the dictionary operators may
 * be provided using the suffix "x" (as in `hashx-ref').  Compared to
 * normal dictionary operators, the "x" forms take extra arguments.
 */
