/* char-name.c - the C names of characters
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/char/char-name.h"


/************************************************************************
 *(h1 "Character Names"
 *    :include ("char/char-name.h"))
 */


/*(c char_name :category variable)
 * extern t_uchar * char_name[256];
 * 
 * `char_name' contains the C names of each character (suitable for
 * use in C character constants or strings).  For example:
 *
 *	char_name['\0']		is	"\\000"
 *	char_name['\n']		is	"\\n"
 *	char_name['\\']		is	"\\\\"
 *	char_name['a']		is	"a"
 */

t_uchar * char_name[] =
{
  "\\000",  "\\001",  "\\002",  "\\003",  "\\004",  "\\005",  "\\006",  "\\007",
  "\\010",  "\\t",  "\\n",  "\\013",  "\\f",  "\\015",  "\\016",  "\\017",
  "\\020",  "\\021",  "\\022",  "\\023",  "\\024",  "\\025",  "\\026",  "\\027",
  "\\030",  "\\031",  "\\032",  "\\033",  "\\034",  "\\035",  "\\036",  "\\037",
  " ",  "!",  "\\\"",  "#",  "$",  "%",  "&",  "'",
  "(",  ")",  "*",  "+",  ",",  "-",  ".",  "/",
  "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
  "8",  "9",  ":",  ";",  "<",  "=",  ">",  "?",
  "@",  "A",  "B",  "C",  "D",  "E",  "F",  "G",
  "H",  "I",  "J",  "K",  "L",  "M",  "N",  "O",
  "P",  "Q",  "R",  "S",  "T",  "U",  "V",  "W",
  "X",  "Y",  "Z",  "[",  "\\\\",  "]",  "^",  "_",
  "`",  "a",  "b",  "c",  "d",  "e",  "f",  "g",
  "h",  "i",  "j",  "k",  "l",  "m",  "n",  "o",
  "p",  "q",  "r",  "s",  "t",  "u",  "v",  "w",
  "x",  "y",  "z",  "{",  "|",  "}",  "~",  "\\177",
  "\\200",  "\\201",  "\\202",  "\\203",  "\\204",  "\\205",  "\\206",  "\\207",
  "\\210",  "\\211",  "\\212",  "\\213",  "\\214",  "\\215",  "\\216",  "\\217",
  "\\220",  "\\221",  "\\222",  "\\223",  "\\224",  "\\225",  "\\226",  "\\227",
  "\\230",  "\\231",  "\\232",  "\\233",  "\\234",  "\\235",  "\\236",  "\\237",
  "\\240",  "\\241",  "\\242",  "\\243",  "\\244",  "\\245",  "\\246",  "\\247",
  "\\250",  "\\251",  "\\252",  "\\253",  "\\254",  "\\255",  "\\256",  "\\257",
  "\\260",  "\\261",  "\\262",  "\\263",  "\\264",  "\\265",  "\\266",  "\\267",
  "\\270",  "\\271",  "\\272",  "\\273",  "\\274",  "\\275",  "\\276",  "\\277",
  "\\300",  "\\301",  "\\302",  "\\303",  "\\304",  "\\305",  "\\306",  "\\307",
  "\\310",  "\\311",  "\\312",  "\\313",  "\\314",  "\\315",  "\\316",  "\\317",
  "\\320",  "\\321",  "\\322",  "\\323",  "\\324",  "\\325",  "\\326",  "\\327",
  "\\330",  "\\331",  "\\332",  "\\333",  "\\334",  "\\335",  "\\336",  "\\337",
  "\\340",  "\\341",  "\\342",  "\\343",  "\\344",  "\\345",  "\\346",  "\\347",
  "\\350",  "\\351",  "\\352",  "\\353",  "\\354",  "\\355",  "\\356",  "\\357",
  "\\360",  "\\361",  "\\362",  "\\363",  "\\364",  "\\365",  "\\366",  "\\367",
  "\\370",  "\\371",  "\\372",  "\\373",  "\\374",  "\\375",  "\\376",  "\\377",
};
