/* tag: Tom Lord Tue Dec  4 14:41:44 2001 (=unidata-size.c)
 */
#include <hackerlab/unicode/unicode.h>


int
main (int argc, char * argv[])
{
  t_unicode c;
  int value;

  c = argv[0][0];

  value = unidata_is_assigned_code_point (c);
  value += (int)unidata_general_category (c);
  value += (int)unidata_decimal_digit_value (c);
  value += (int)unidata_bidi_category (c);
  value += (int)unidata_is_mirrored (c);
  value += (int)unidata_canonical_combining_class (c);
  value += (int)unidata_to_upper (c);
  value += (int)unidata_to_lower (c);
  value += (int)unidata_to_title (c);
  value += (int)unidata_character_decomposition_mapping (c)->type;
  value += (int)uni_blocks[0].start;
  value += bits_population (uni_general_category_bitset (uni_general_category_Sm));

  return value;
}
