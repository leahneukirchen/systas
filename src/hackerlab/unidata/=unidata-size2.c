/* tag: Tom Lord Tue Dec  4 14:41:44 2001 (=unidata-size2.c)
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
  value += (int)unidata_canonical_combining_class (c);
  value += (int)unidata_to_upper (c);
  value += (int)unidata_to_lower (c);
  value += (int)unidata_to_title (c);
  value += (int)uni_blocks[0].start;

  return value;
}
