/* char-class.c - character classifications
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/char/char-class.h"


/************************************************************************
 *(h1 "Character Classes"
 *    :include ("char/char-class.h"))
 *
 * The character-class macros assign each byte to various categories of
 * characters such as upper-case and lower-case letters.
 * 
 * These functions are based on the ASCII character set.  They are not
 * locale-sensative.
 *
 */



#define cset(Q) \
	Q(0),	Q(1),	Q(2),	Q(3),	Q(4),	Q(5),	Q(6),	Q(7), \
	Q(8),	Q(9),	Q(10),	Q(11),	Q(12),	Q(13),	Q(14),	Q(15), \
	Q(16),	Q(17),	Q(18),	Q(19),	Q(20),	Q(21),	Q(22),	Q(23), \
	Q(24),	Q(25),	Q(26),	Q(27),	Q(28),	Q(29),	Q(30),	Q(31), \
	Q(32),	Q(33),	Q(34),	Q(35),	Q(36),	Q(37),	Q(38),	Q(39), \
	Q(40),	Q(41),	Q(42),	Q(43),	Q(44),	Q(45),	Q(46),	Q(47), \
	Q(48),	Q(49),	Q(50),	Q(51),	Q(52),	Q(53),	Q(54),	Q(55), \
	Q(56),	Q(57),	Q(58),	Q(59),	Q(60),	Q(61),	Q(62),	Q(63), \
	Q(64),	Q(65),	Q(66),	Q(67),	Q(68),	Q(69),	Q(70),	Q(71), \
	Q(72),	Q(73),	Q(74),	Q(75),	Q(76),	Q(77),	Q(78),	Q(79), \
	Q(80),	Q(81),	Q(82),	Q(83),	Q(84),	Q(85),	Q(86),	Q(87), \
	Q(88),	Q(89),	Q(90),	Q(91),	Q(92),	Q(93),	Q(94),	Q(95), \
	Q(96),	Q(97),	Q(98),	Q(99),	Q(100),	Q(101),	Q(102),	Q(103), \
	Q(104),	Q(105),	Q(106),	Q(107),	Q(108),	Q(109),	Q(110),	Q(111), \
	Q(112),	Q(113),	Q(114),	Q(115),	Q(116),	Q(117),	Q(118),	Q(119), \
	Q(120),	Q(121),	Q(122),	Q(123),	Q(124),	Q(125),	Q(126),	Q(127), \
	Q(128),	Q(129),	Q(130),	Q(131),	Q(132),	Q(133),	Q(134),	Q(135), \
	Q(136),	Q(137),	Q(138),	Q(139),	Q(140),	Q(141),	Q(142),	Q(143), \
	Q(144),	Q(145),	Q(146),	Q(147),	Q(148),	Q(149),	Q(150),	Q(151), \
	Q(152),	Q(153),	Q(154),	Q(155),	Q(156),	Q(157),	Q(158),	Q(159), \
	Q(160),	Q(161),	Q(162),	Q(163),	Q(164),	Q(165),	Q(166),	Q(167), \
	Q(168),	Q(169),	Q(170),	Q(171),	Q(172),	Q(173),	Q(174),	Q(175), \
	Q(176),	Q(177),	Q(178),	Q(179),	Q(180),	Q(181),	Q(182),	Q(183), \
	Q(184),	Q(185),	Q(186),	Q(187),	Q(188),	Q(189),	Q(190),	Q(191), \
	Q(192),	Q(193),	Q(194),	Q(195),	Q(196),	Q(197),	Q(198),	Q(199), \
	Q(200),	Q(201),	Q(202),	Q(203),	Q(204),	Q(205),	Q(206),	Q(207), \
	Q(208),	Q(209),	Q(210),	Q(211),	Q(212),	Q(213),	Q(214),	Q(215), \
	Q(216),	Q(217),	Q(218),	Q(219),	Q(220),	Q(221),	Q(222),	Q(223), \
	Q(224),	Q(225),	Q(226),	Q(227),	Q(228),	Q(229),	Q(230),	Q(231), \
	Q(232),	Q(233),	Q(234),	Q(235),	Q(236),	Q(237),	Q(238),	Q(239), \
	Q(240),	Q(241),	Q(242),	Q(243),	Q(244),	Q(245),	Q(246),	Q(247), \
	Q(248),	Q(249),	Q(250),	Q(251),	Q(252),	Q(253),	Q(254),	Q(255)



#define as_lower(X)	((((X) >= 'A') && ((X) <= 'Z')) \
			 ? ((X) - 'A' + 'a') \
			 : (X))

#define as_upper(X)	((((X) >= 'a') && ((X) <= 'z')) \
			 ? ((X) - 'a' + 'A') \
			 : (X))

t_uchar char__to_lower_table[] = { cset(as_lower) };
t_uchar char__to_upper_table[] = { cset(as_upper) };



#define upper_test(C)		(((C) >= 'A') && ((C) <= 'Z'))
#define lower_test(C)		(((C) >= 'a') && ((C) <= 'z'))
#define alpha_test(C)		(upper_test(C) || lower_test(C))
#define digit_test(C)		(((C) >= '0') && ((C) <= '9'))
#define alnum_test(C)		(alpha_test(C) || digit_test(C))
#define control_test(C)		((C) < 32)
#define space_test(C)		(((C) == '\n') || ((C) == '\t') || ((C) == '\f') || ((C) == ' ') || ((C) == '\r') || ((C) == '\v'))
#define graph_test(C)		(((C) >= 33) && ((C) < 127))
#define printable_test(C)	(graph_test(C) || ((C) == ' '))
#define c_id_test(C)		(alnum_test(C) || ((C) == '_'))
#define xdigit_test(C)		(   digit_test(C) \
				 || (((C) >= 'a') && ((C) <= 'f')) \
				 || (((C) >= 'A') && ((C) <= 'F')))
#define odigit_test(C)		(((C) >= '0') && ((C) <= '7'))
#define punct_test(C)		(   (((C) >= 33) && ((C) <= 47)) \
				 || (((C) >= 58) && ((C) <= 64)) \
				 || (((C) >= 91) && ((C) <= 96)) \
				 || (((C) >= 123) && ((C) <= 126)))
#define blank_test(C)		(((C) == ' ') || ((C) == '\t'))

#define upper_bit(C)		(upper_test(C) ? char_class_upper : 0)
#define lower_bit(C)		(lower_test(C) ? char_class_lower : 0)
#define alpha_bit(C)		(alpha_test(C) ? char_class_alpha : 0)
#define digit_bit(C)		(digit_test(C) ? char_class_digit : 0)
#define alnum_bit(C)		(alnum_test(C) ? char_class_alnum : 0)
#define control_bit(C)		(control_test(C) ? char_class_control : 0)
#define printable_bit(C)	(printable_test(C) ? char_class_printable : 0)
#define space_bit(C)		(space_test(C) ? char_class_space : 0)
#define graph_bit(C)		(graph_test(C) ? char_class_graph : 0)
#define c_id_bit(C)		(c_id_test(C) ? char_class_c_id : 0)
#define xdigit_bit(C)		(xdigit_test(C) ? char_class_xdigit : 0)
#define odigit_bit(C)		(odigit_test(C) ? char_class_odigit : 0)
#define punct_bit(C)		(punct_test(C) ? char_class_punct : 0)
#define blank_bit(C)		(blank_test(C) ? char_class_blank : 0)


#define class_bits(C)		(  upper_bit(C) \
				 | lower_bit(C) \
				 | alpha_bit(C) \
				 | digit_bit(C) \
				 | alnum_bit(C) \
				 | control_bit(C) \
				 | printable_bit(C) \
				 | space_bit(C) \
				 | graph_bit(C) \
				 | c_id_bit(C) \
				 | xdigit_bit(C) \
				 | odigit_bit(C) \
				 | punct_bit(C) \
				 | blank_bit(C))


#define digit_value_n(C)	((digit_test (C) \
				  ? ((C) - '0') \
				  : (upper_test (C) \
				     ? (10 + (C) - 'A') \
				     : ((lower_test (C) \
					 ? (10 + (C) - 'a') \
					 : -1)))))

unsigned short char__class_table[] =
{
  cset (class_bits)
};

short char__digit_value_table [] =
{
  cset (digit_value_n)
};


#if 0

/*(c char_is_upper :category macro)
 * int char_is_upper (t_uchar x);
 * 
 * Return 1 if `x' is an upper case letter, 0 otherwise.
 */
int
char_is_upper (t_uchar x)
{ macro }


/*(c char_is_lower :category macro)
 * int char_is_lower (t_uchar x);
 * 
 * Return 1 if `x' is a lower case letter, 0 otherwise.
 */
int
char_is_lower (t_uchar x)
{ macro }


/*(c char_is_alpha :category macro)
 * int char_is_alpha (t_uchar x);
 * 
 * Return 1 if `x' is an upper or lower case letter, 0 otherwise.
 */
int
char_is_alpha (t_uchar x)
{ macro }


/*(c char_is_digit :category macro)
 * int char_is_digit (t_uchar x);
 * 
 * Return 1 if `x' is a decimal digit, 0 otherwise.
 */
int
char_is_digit (t_uchar x)
{ macro }


/*(c char_is_alnum :category macro)
 * int char_is_alnum (t_uchar x);
 * 
 * Return 1 if `x' is an upper or lower case letter or digit, 0 otherwise.
 */
int
char_is_alnum (t_uchar x)
{ macro }


/*(c char_is_control :category macro)
 * int char_is_control (t_uchar x);
 * 
 * Return 1 if `x' is a control character, 0 otherwise.
 */
int
char_is_control (t_uchar x)
{ macro }


/*(c char_is_printable :category macro)
 * int char_is_printable (t_uchar x);
 * 
 * Return 1 if `x' is a printable character, 0 otherwise.
 */
int
char_is_printable (t_uchar x)
{ macro }


/*(c char_is_space :category macro)
 * int char_is_space (t_uchar x);
 * 
 * Return 1 if `x' is a space character, 0 otherwise.
 */
int
char_is_space (t_uchar x)
{ macro }


/*(c char_is_graph :category macro)
 * int char_is_graph (t_uchar x);
 * 
 * Return 1 if `x' is a non-space printable character, 0 otherwise.
 */
int
char_is_graph (t_uchar x)
{ macro }


/*(c char_is_c_id :category macro)
 * int char_is_c_id (t_uchar x);
 * 
 * Return 1 if `x' is an alpha-numeric character or '_', 0 otherwise.
 */
int
char_is_c_id (t_uchar x)
{ macro }


/*(c char_is_xdigit :category macro)
 * int char_is_xdigit (t_uchar x);
 * 
 * Return 1 if `x' is a hexadecimal digit, 0 otherwise.
 */
int
char_is_xdigit (t_uchar x)
{ macro }

/*(c char_is_odigit :category macro)
 * int char_is_odigit (t_uchar x);
 * 
 * Return 1 if `x' is an octal digit, 0 otherwise.
 */
int
char_is_odigit (t_uchar x)
{ macro }


/*(c char_is_punct :category macro)
 * int char_is_punct (t_uchar x);
 * 
 * Return 1 if `x' is a punctuation character, 0 otherwise.
 */
int
char_is_punct (t_uchar x)
{ macro }


#endif
