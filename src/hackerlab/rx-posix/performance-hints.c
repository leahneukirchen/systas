/* tag: Tom Lord Tue Dec  4 14:41:38 2001 (performance-hints.c)
 */
/* performance-hints.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


/************************************************************************
 *(h0 "Hints for Obtaining Good Performance")
 * 
 * |performance hints|
 * This chapter contains some hints for achieving good performance
 * when using the Posix regexp functions.
 * 
 */

/************************************************************************
 *(h1 "The Complexity of Posix Regexps")
 * 
 * |complexity of regexps|
 * |algorithmic complexity of regexps|
 * Sometimes, programmers code as if "regexp searches are always
 * fast".  For Posix regexps, that is simply not the case, regardless
 * of what implementation is being used.
 * 
 * The difficulty can be understood as follows:
 * 
 * Consider, first, the problem of comparing an entire string to a
 * Posix regexp.  We want to know if the entire string matches.  If
 * the string matches, we want to know the positions of parenthesized
 * subexpressions.  This is the *Posix match problem*. |Posix match problem|
 * 
 * The computational complexity of the best known algorithms for the
 * Posix match problem is a function of three things: the particular
 * regexp being matched, the length of the string being matched, and
 * the contents of the string being matched.  The complexity (the
 * number of steps needed to complete a match) is a polynomial
 * function of the length of the string being matched.  The degree of
 * the polynomial is determined by the regexp and is arbitrarily large
 * (limited only by the length of the regexp itself).  There are
 * optimizations that can reduce the degree of that polynomial, but
 * those optimizations can be often be thwarted by carefully choosing
 * the regexp and/or contents of the string being matched.
 * 
 * The problems only get worse when using the Posix function
 * `regexec'.  `regexec' doesn't try to match an entire string: it
 * searches for a matching substring (the *Posix search problem*). |Posix search problem|
 * There are no universally applicable short-cuts for that search:
 * `regexec' must potentially examine very possible substring.  To
 * search a string of length `N', `regexec' might have to examine
 * `O(N^3)' substrings.  Various heuristic optimizations can usually
 * reduce the size of the search, but not always.
 * 
 * In short, though programmers sometimes think "regexp searches are
 * always fast", in fact, although "many regexp searches are fast", it is also
 * the case that "some regexp searches are unacceptably slow".
 */


/*(h1 "Avoiding Regexp Based Denial of Service Attacks")
 * 
 * |denial of service attacks|
 * 
 * The complexity of Posix regexp matching is a serious concern for
 * some applications.  If regexps and target strings are part of the
 * input to your program, some combinations of inputs can cause your
 * program to run for a ridiculously long time without producing a
 * useful result.  If your application provides a critical service,
 * this can be the basis of a denial of service attack.  Even if your
 * application is not critical, this can be the basis of confusing and
 * annoying behavior.
 * 
 * A possible solution to some kinds of denial of service attacks
 * |denial of service attacks| is to set a time limit for matches and
 * interrupt matches that are taking too long.  (See xref:"Escaping
 * Long-Running Matches".)
 */



/************************************************************************
 *(h1 "Using Regexps Carefully")
 * 
 * |Posix match problem| |cache (DFA)| |DFA cache|
 * For a careful choice of regexps, with no subexpression position
 * reporting, the *Posix match problem* can be solved quickly: an
 * expected case of `O(n)' steps for a string of length `n' with a
 * worst case of `O(n*k)' where `k' is the length of the regexp.  (the
 * effectiveness of the DFA cache determines whether expected or worst
 * case behavior is observed)
 * 
 * Rx helps achieve that level of performance in several ways.
 * 
 * \1./ True regexps |true regexps| are always that fast.  (See xref:"Regexps versus
 * Regular Expressions".)  Whenever possible, use anonymous subexpressions |anonymous subexpression|
 * |[[:(...:)]]|
 * rather than parenthesized subexpressions |parenthesized subexpression| |(...)|.
 * (See xref:"Anonymous Subexpressions".)
 * 
 * \2./ Expressions compiled with the non-standard regcomp flag `REG_DFA_ONLY' are
 * always that fast.  (See xref:"regcomp".)
 * 
 * \3./ Fast matches are obtained for regexps which are not true
 * regexps but which contain no back references |back references| |\n| (`\n'), anchors |anchors| |^| |$| (`^'
 * or `$') or iterated subexpressions |iterated subexpressions| |{...}| (`RE{n,m}' ) if 0 is passed for
 * the `preg' argument to regexec (implying that subexpression
 * positionss will not be returend)
 * 
 * Rx optimizes the Posix search problem too:
 * 
 * 
 * \1./ Anchors (`^' and `$') can reduce the number of substrings searched.  
 * Fast solutions to the Posix match problem are usually possible
 * even if a pattern begins and/or ends with anchors.
 * 
 * \2./ If a matching string must begin with one of a small set of characters,
 * substrings which do not begin with those characters are not searched.
 * 
 * \3./ If a regexp matches only strings of a particular length, only
 * substrings of that exact length are searched.
 * 
 */



/************************************************************************
 *(h1 "A Strategy for Searching Long Strings")
 * 
 * |long searches|
 * The ideas outlined above lead to the following: If your application
 * searches long strings for regexp matches, and wants to know the
 * positions of matching subexpressions, a two part strategy may speed
 * things up:
 * 
 * \First,/ search for a matching substring without asking for
 * subexpression positions (pass 0 for `preg').
 * 
 * \Second,/ having found a matching substring, re-match that
 * substring, asking for subexpression positions.
 * 
 * Note that this strategy doesn't help if the regexp contains back
 * references, and it doesn't guarantee that searching will be fast.
 */


