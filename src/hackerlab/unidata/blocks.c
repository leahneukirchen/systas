/* blocks.c - standard unicode blocks
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/unidata/blocks.h"



const struct uni_block uni_blocks[] =
{
  {
    "Basic Latin",
    0x0000,
    0x007F,
  },

  {
    "Latin-1 Supplement",
    0x0080,
    0x00FF,
  },

  {
    "Latin Extended-A",
    0x0100,
    0x017F,
  },

  {
    "Latin Extended-B",
    0x0180,
    0x024F,
  },

  {
    "IPA Extensions",
    0x0250,
    0x02AF,
  },

  {
    "Spacing Modifier Letters",
    0x02B0,
    0x02FF,
  },

  {
    "Combining Diacritical Marks",
    0x0300,
    0x036F,
  },

  {
    "Greek",
    0x0370,
    0x03FF,
  },

  {
    "Cyrillic",
    0x0400,
    0x04FF,
  },

  {
    "Armenian",
    0x0530,
    0x058F,
  },

  {
    "Hebrew",
    0x0590,
    0x05FF,
  },

  {
    "Arabic",
    0x0600,
    0x06FF,
  },

  {
    "Syriac  ",
    0x0700,
    0x074F,
  },

  {
    "Thaana",
    0x0780,
    0x07BF,
  },

  {
    "Devanagari",
    0x0900,
    0x097F,
  },

  {
    "Bengali",
    0x0980,
    0x09FF,
  },

  {
    "Gurmukhi",
    0x0A00,
    0x0A7F,
  },

  {
    "Gujarati",
    0x0A80,
    0x0AFF,
  },

  {
    "Oriya",
    0x0B00,
    0x0B7F,
  },

  {
    "Tamil",
    0x0B80,
    0x0BFF,
  },

  {
    "Telugu",
    0x0C00,
    0x0C7F,
  },

  {
    "Kannada",
    0x0C80,
    0x0CFF,
  },

  {
    "Malayalam",
    0x0D00,
    0x0D7F,
  },

  {
    "Sinhala",
    0x0D80,
    0x0DFF,
  },

  {
    "Thai",
    0x0E00,
    0x0E7F,
  },

  {
    "Lao",
    0x0E80,
    0x0EFF,
  },

  {
    "Tibetan",
    0x0F00,
    0x0FFF,
  },

  {
    "Myanmar ",
    0x1000,
    0x109F,
  },

  {
    "Georgian",
    0x10A0,
    0x10FF,
  },

  {
    "Hangul Jamo",
    0x1100,
    0x11FF,
  },

  {
    "Ethiopic",
    0x1200,
    0x137F,
  },

  {
    "Cherokee",
    0x13A0,
    0x13FF,
  },

  {
    "Unified Canadian Aboriginal Syllabics",
    0x1400,
    0x167F,
  },

  {
    "Ogham",
    0x1680,
    0x169F,
  },

  {
    "Runic",
    0x16A0,
    0x16FF,
  },

  {
    "Khmer",
    0x1780,
    0x17FF,
  },

  {
    "Mongolian",
    0x1800,
    0x18AF,
  },

  {
    "Latin Extended Additional",
    0x1E00,
    0x1EFF,
  },

  {
    "Greek Extended",
    0x1F00,
    0x1FFF,
  },

  {
    "General Punctuation",
    0x2000,
    0x206F,
  },

  {
    "Superscripts and Subscripts",
    0x2070,
    0x209F,
  },

  {
    "Currency Symbols",
    0x20A0,
    0x20CF,
  },

  {
    "Combining Marks for Symbols",
    0x20D0,
    0x20FF,
  },

  {
    "Letterlike Symbols",
    0x2100,
    0x214F,
  },

  {
    "Number Forms",
    0x2150,
    0x218F,
  },

  {
    "Arrows",
    0x2190,
    0x21FF,
  },

  {
    "Mathematical Operators",
    0x2200,
    0x22FF,
  },

  {
    "Miscellaneous Technical",
    0x2300,
    0x23FF,
  },

  {
    "Control Pictures",
    0x2400,
    0x243F,
  },

  {
    "Optical Character Recognition",
    0x2440,
    0x245F,
  },

  {
    "Enclosed Alphanumerics",
    0x2460,
    0x24FF,
  },

  {
    "Box Drawing",
    0x2500,
    0x257F,
  },

  {
    "Block Elements",
    0x2580,
    0x259F,
  },

  {
    "Geometric Shapes",
    0x25A0,
    0x25FF,
  },

  {
    "Miscellaneous Symbols",
    0x2600,
    0x26FF,
  },

  {
    "Dingbats",
    0x2700,
    0x27BF,
  },

  {
    "Braille Patterns",
    0x2800,
    0x28FF,
  },

  {
    "CJK Radicals Supplement",
    0x2E80,
    0x2EFF,
  },

  {
    "Kangxi Radicals",
    0x2F00,
    0x2FDF,
  },

  {
    "Ideographic Description Characters",
    0x2FF0,
    0x2FFF,
  },

  {
    "CJK Symbols and Punctuation",
    0x3000,
    0x303F,
  },

  {
    "Hiragana",
    0x3040,
    0x309F,
  },

  {
    "Katakana",
    0x30A0,
    0x30FF,
  },

  {
    "Bopomofo",
    0x3100,
    0x312F,
  },

  {
    "Hangul Compatibility Jamo",
    0x3130,
    0x318F,
  },

  {
    "Kanbun",
    0x3190,
    0x319F,
  },

  {
    "Bopomofo Extended",
    0x31A0,
    0x31BF,
  },

  {
    "Enclosed CJK Letters and Months",
    0x3200,
    0x32FF,
  },

  {
    "CJK Compatibility",
    0x3300,
    0x33FF,
  },

  {
    "CJK Unified Ideographs Extension A",
    0x3400,
    0x4DB5,
  },

  {
    "CJK Unified Ideographs",
    0x4E00,
    0x9FFF,
  },

  {
    "Yi Syllables",
    0xA000,
    0xA48F,
  },

  {
    "Yi Radicals",
    0xA490,
    0xA4CF,
  },

  {
    "Hangul Syllables",
    0xAC00,
    0xD7A3,
  },

  {
    "High Surrogates",
    0xD800,
    0xDB7F,
  },

  {
    "High Private Use Surrogates",
    0xDB80,
    0xDBFF,
  },

  {
    "Low Surrogates",
    0xDC00,
    0xDFFF,
  },

  {
    "Private Use",
    0xE000,
    0xF8FF,
  },

  {
    "CJK Compatibility Ideographs",
    0xF900,
    0xFAFF,
  },

  {
    "Alphabetic Presentation Forms",
    0xFB00,
    0xFB4F,
  },

  {
    "Arabic Presentation Forms-A",
    0xFB50,
    0xFDFF,
  },

  {
    "Combining Half Marks",
    0xFE20,
    0xFE2F,
  },

  {
    "CJK Compatibility Forms",
    0xFE30,
    0xFE4F,
  },

  {
    "Small Form Variants",
    0xFE50,
    0xFE6F,
  },

  {
    "Arabic Presentation Forms-B",
    0xFE70,
    0xFEFE,
  },

  {
    "Specials",
    0xFEFF,
    0xFEFF,
  },

  {
    "Halfwidth and Fullwidth Forms",
    0xFF00,
    0xFFEF,
  },

  {
    "Specials",
    0xFFF0,
    0xFFFD,
  },

  {0, 0, 0}
};

const int n_uni_blocks = (sizeof (uni_blocks) / sizeof(struct uni_block)) - 1;

