/* hash-utils.c - computing hash values
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/machine/alignment.h"
#include "hackerlab/hash/hash-utils.h"


/************************************************************************
 *(h1 "Hash Utilities"
 *    :includes ("hackerlab/hash/hash-utils.h"))
 * 
 * The functions in this section provide tools useful for computing 
 * hash values.
 */



#define TWO_BIT_BYTE(A,B)	((1<<(A)) | (1<<(B)))

#if MACHINE_SIZEOF_LONG == 4
static int word4_bit_selection[4][4] =
{
  { TWO_BIT_BYTE (0,7), TWO_BIT_BYTE (1,6), TWO_BIT_BYTE (2,5), TWO_BIT_BYTE (3,4) },
  { TWO_BIT_BYTE (1,0), TWO_BIT_BYTE (2,7), TWO_BIT_BYTE (3,6), TWO_BIT_BYTE (4,5) },
  { TWO_BIT_BYTE (2,1), TWO_BIT_BYTE (3,0), TWO_BIT_BYTE (4,7), TWO_BIT_BYTE (5,6) },
  { TWO_BIT_BYTE (3,2), TWO_BIT_BYTE (4,1), TWO_BIT_BYTE (5,0), TWO_BIT_BYTE (6,7) }
};

#elif MACHINE_SIZEOF_LONG == 8

#define OBB(A,K)		(1 << ((A + K) % 8))

static int word8_bit_selection[8][8] =
{
  {OBB (6, 0), OBB (4, 0), OBB (7, 0), OBB (3, 0), OBB (0, 0), OBB (1, 0), OBB (5, 0), OBB (2, 0)},
  {OBB (6, 1), OBB (4, 1), OBB (7, 1), OBB (3, 1), OBB (0, 1), OBB (1, 1), OBB (5, 1), OBB (2, 1)},
  {OBB (6, 2), OBB (4, 2), OBB (7, 2), OBB (3, 2), OBB (0, 2), OBB (1, 2), OBB (5, 2), OBB (2, 2)},
  {OBB (6, 3), OBB (4, 3), OBB (7, 3), OBB (3, 3), OBB (0, 3), OBB (1, 3), OBB (5, 3), OBB (2, 3)},
  {OBB (6, 4), OBB (4, 4), OBB (7, 4), OBB (3, 4), OBB (0, 4), OBB (1, 4), OBB (5, 4), OBB (2, 4)},
  {OBB (6, 5), OBB (4, 5), OBB (7, 5), OBB (3, 5), OBB (0, 5), OBB (1, 5), OBB (5, 5), OBB (2, 5)},
  {OBB (6, 6), OBB (4, 6), OBB (7, 6), OBB (3, 6), OBB (0, 6), OBB (1, 6), OBB (5, 6), OBB (2, 6)},
  {OBB (6, 7), OBB (4, 7), OBB (7, 7), OBB (3, 7), OBB (0, 7), OBB (1, 7), OBB (5, 7), OBB (2, 7)},
};

#else
#error "weird sizeof (long)"
#endif

static unsigned long shuffled_bytes[] =
{
  245, 184, 171, 36, 93, 194, 192, 143, 207, 89, 63, 175, 203, 231, 47, 238,
  103, 67, 176, 102, 80, 133, 24, 155, 91, 141, 234, 58, 44, 191, 218, 157,
  13, 168, 160, 113, 211, 213, 252, 236, 2, 19, 21, 148, 111, 251, 165, 74,
  124, 25, 181, 210, 250, 195, 235, 97, 185, 1, 179, 198, 105, 101, 5, 220,
  35, 162, 142, 41, 200, 209, 224, 71, 201, 134, 69, 48, 65, 170, 72, 167,
  145, 205, 28, 88, 215, 81, 214, 78, 118, 26, 123, 84, 140, 49, 45, 8,
  7, 107, 227, 60, 59, 32, 30, 82, 31, 189, 131, 17, 66, 239, 64, 10,
  149, 40, 130, 146, 54, 147, 9, 114, 4, 254, 241, 116, 110, 249, 57, 233,
  37, 55, 206, 100, 177, 119, 139, 158, 108, 75, 94, 23, 186, 152, 244, 27,
  38, 33, 188, 87, 76, 166, 228, 52, 120, 99, 247, 174, 51, 183, 3, 161,
  246, 135, 14, 178, 11, 216, 77, 172, 122, 154, 39, 253, 104, 34, 164, 230,
  219, 242, 68, 151, 180, 115, 173, 73, 212, 90, 125, 29, 22, 221, 56, 121,
  255, 204, 83, 169, 182, 112, 96, 187, 20, 106, 79, 15, 61, 223, 70, 85,
  53, 197, 217, 232, 196, 95, 136, 150, 243, 109, 129, 202, 208, 237, 144, 156,
  86, 127, 62, 248, 138, 229, 153, 226, 240, 199, 50, 12, 193, 98, 137, 126,
  0, 159, 222, 18, 163, 117, 190, 46, 225, 132, 16, 43, 128, 42, 92, 6
};


static unsigned long shuffled_bytes2[] =
{
  0, 202, 239, 108, 121, 122, 197, 193, 220, 75, 79, 208, 162, 157, 40, 95,
  138, 223, 51, 42, 151, 165, 3, 133, 18, 227, 232, 125, 114, 41, 10, 23,
  178, 29, 43, 106, 167, 203, 61, 100, 250, 247, 1, 253, 105, 81, 205, 82,
  63, 149, 92, 89, 117, 221, 96, 181, 177, 252, 152, 135, 64, 150, 28, 132,
  236, 46, 5, 7, 255, 38, 243, 175, 248, 242, 171, 212, 67, 186, 57, 86,
  15, 211, 47, 218, 113, 207, 134, 11, 128, 97, 169, 52, 4, 101, 198, 159,
  54, 224, 222, 161, 91, 182, 238, 209, 139, 76, 142, 254, 36, 80, 111, 219,
  229, 26, 93, 22, 9, 195, 185, 200, 226, 126, 107, 237, 183, 251, 44, 94,
  234, 166, 112, 53, 188, 120, 30, 213, 68, 190, 25, 78, 102, 244, 37, 153,
  20, 141, 103, 90, 62, 98, 144, 21, 155, 230, 34, 189, 176, 59, 191, 168,
  130, 87, 201, 214, 192, 73, 164, 246, 13, 124, 140, 58, 110, 14, 145, 137,
  179, 217, 85, 2, 19, 8, 154, 104, 196, 84, 65, 147, 206, 16, 123, 160,
  240, 60, 127, 235, 99, 199, 216, 6, 204, 156, 118, 225, 56, 83, 158, 143,
  71, 66, 163, 173, 116, 172, 228, 249, 49, 48, 69, 210, 184, 70, 74, 241,
  88, 24, 194, 174, 35, 17, 32, 33, 231, 77, 148, 119, 233, 27, 136, 115,
  39, 170, 12, 180, 31, 55, 187, 50, 109, 131, 45, 129, 146, 215, 72, 245
};

#ifdef __GNUC__
#define INLINE inline
#else
#define INLINE
#endif

static INLINE unsigned long
shuffled_word_byte (unsigned long word, int byte_n)
{
#if MACHINE_SIZEOF_LONG == 4
  /* Pick two bits from each byte of `word'.
   * Combine those bits using '^' to form a byte.
   * For extra randomness, translate that byte using `shuffled_bytes'.
   */
  return shuffled_bytes[(  ((word & 0xff) & word4_bit_selection[byte_n][0])
			 ^ (((word >> 8) & 0xff) & word4_bit_selection[byte_n][1])
			 ^ (((word >> 16) & 0xff) & word4_bit_selection[byte_n][2])
			 ^ (((word >> 24) & 0xff) & word4_bit_selection[byte_n][3]))];
#elif MACHINE_SIZEOF_LONG == 8
  /* Pick one bit from each byte of `word'.
   * Combine those bits using '^' to form a byte.
   * For extra randomness, translate that byte using `shuffled_bytes'.
   */
  return shuffled_bytes[(  ((word & 0xff) & word8_bit_selection[byte_n][0])
			 ^ (((word >> 8) & 0xff) & word8_bit_selection[byte_n][1])
			 ^ (((word >> 16) & 0xff) & word8_bit_selection[byte_n][2])
			 ^ (((word >> 24) & 0xff) & word8_bit_selection[byte_n][3])
			 ^ (((word >> 32) & 0xff) & word8_bit_selection[byte_n][4])
			 ^ (((word >> 40) & 0xff) & word8_bit_selection[byte_n][5])
			 ^ (((word >> 48) & 0xff) & word8_bit_selection[byte_n][6])
			 ^ (((word >> 56) & 0xff) & word8_bit_selection[byte_n][7]))];
#else
#error "weird sizeof(long)"
#endif
}



/*(c hash_ul)
 * unsigned long hash_ul (unsigned long n);
 * 
 * Generate a hash value from an integer.
 * 
 * This function is slow, but attempts to give a good distribution of
 * hash values even for a series of `n' which are not particularly
 * random.
 * 
 * `slow' means that the function does rougly `3 * sizeof (n)' array
 * look-ups and lots of bit twiddling.
 */
unsigned long
hash_ul (unsigned long n)
{
#if MACHINE_SIZEOF_LONG == 4
  /* Generate four fairly random bytes from the four bytes of `n'.
   */
  unsigned long b0 = shuffled_word_byte(n, 0);
  unsigned long b1 = shuffled_word_byte(n, 1);
  unsigned long b2 = shuffled_word_byte(n, 2);
  unsigned long b3 = shuffled_word_byte(n, 3);
  
  unsigned long c0;
  unsigned long c1;
  unsigned long c2;
  unsigned long c3;
  
  /* For good measure, spread the randomness of
   * those bytes aroud, each contributing to the
   * choice of three out of four random bytes.
   */
  c0 = shuffled_bytes2[b1 ^ b2 ^ b3];
  c1 = shuffled_bytes2[b0 ^ b2 ^ b3];
  c2 = shuffled_bytes2[b0 ^ b1 ^ b3];
  c3 = shuffled_bytes2[b0 ^ b1 ^ b2];
  
  /* Combine the resulting bytes to form a hash value.
   */
  return (c0 << 24) | (c1 << 16) | (c2 << 8) | c3;

#elif MACHINE_SIZEOF_LONG == 8
  /* Generate eight fairly random bytes from the four bytes of `n'.
   */
  unsigned long b0 = shuffled_word_byte(n, 0);
  unsigned long b1 = shuffled_word_byte(n, 1);
  unsigned long b2 = shuffled_word_byte(n, 2);
  unsigned long b3 = shuffled_word_byte(n, 3);
  unsigned long b4 = shuffled_word_byte(n, 4);
  unsigned long b5 = shuffled_word_byte(n, 5);
  unsigned long b6 = shuffled_word_byte(n, 6);
  unsigned long b7 = shuffled_word_byte(n, 7);
  
  unsigned long c0;
  unsigned long c1;
  unsigned long c2;
  unsigned long c3;
  unsigned long c4;
  unsigned long c5;
  unsigned long c6;
  unsigned long c7;
  
  /* For good measure, spread the randomness of
   * those bytes aroud, each contributing to the
   * choice of seven out of eight random bytes.
   */
  c0 = shuffled_bytes2[b1 ^ b2 ^ b3 ^ b4 ^ b5 ^ b6 ^ b7];
  c1 = shuffled_bytes2[b0 ^ b2 ^ b3 ^ b4 ^ b5 ^ b6 ^ b7];
  c2 = shuffled_bytes2[b0 ^ b1 ^ b3 ^ b4 ^ b5 ^ b6 ^ b7];
  c3 = shuffled_bytes2[b0 ^ b1 ^ b2 ^ b4 ^ b5 ^ b6 ^ b7];
  c4 = shuffled_bytes2[b0 ^ b1 ^ b2 ^ b3 ^ b5 ^ b6 ^ b7];
  c5 = shuffled_bytes2[b0 ^ b1 ^ b2 ^ b3 ^ b4 ^ b6 ^ b7];
  c6 = shuffled_bytes2[b0 ^ b1 ^ b2 ^ b3 ^ b4 ^ b5 ^ b7];
  c7 = shuffled_bytes2[b0 ^ b1 ^ b2 ^ b3 ^ b4 ^ b5 ^ b6];
  
  /* Combine the resulting bytes to form a hash value.
   */
  return (c0 << 56) | (c1 << 48) | (c2 << 40) | (c3 << 32) | (c4 << 24) | (c5 << 16) | (c6 << 8) | c7;

#else
#error "weird sizeof (long)"
#endif
}





/*(c hash_pointers)
 * unsigned long hash_pointers (void * elts, size_t n_elts);
 * 
 * Compute a hash value from an array of pointers.
 * 
 * This function is slow, but attempts to give a good distribution of
 * hash values even for a series of pointers which are not
 * particularly random.  Usually, pointers are not particularly
 * random.
 * 
 * `slow' means that the function does roughly `3 * sizeof (n)' array
 * look-ups and lots of bit twiddling, per pointer.
 * 
 */
unsigned long
hash_pointers (void * elts, size_t n_elts)
{
  size_t x;
  unsigned long hash;

  hash = 0xdec0ded;
  for (x = 0; x < n_elts; ++x)
    {
      hash ^= hash_ul (((unsigned long *)elts)[x]);
    }
  return hash;
}




/*(c hash_mem)
 * unsigned long hash_mem (t_uchar * elts, size_t n_bytes);
 * 
 * Compute a hash value from an array of bytes.
 * 
 * This function is slow, but attempts to give a good distribution of
 * hash values even for a series of bytes which are not particularly
 * random.
 * 
 * `slow' means that the function does roughly `3 * sizeof (n)' array
 * look-ups and lots of bit twiddling, per `sizeof (unsigned long)'
 * bytes.
 * 
 */
unsigned long
hash_mem (t_uchar * elts, size_t n_elts)
{
  size_t x;
  unsigned long hash;

  hash = 0xde7a115;

  for (x = 0; (x < n_elts) && (MACHINE_ALIGNMENT - 1); ++x)
    hash ^= hash_ul ((unsigned long)elts[x]);

  while ((n_elts - x) >= sizeof (unsigned long))
    {
      hash ^= hash_ul (*(unsigned long *)(elts + x));
      x += sizeof (unsigned long);
    }
  
  while (x < n_elts)
    {
      hash ^= hash_ul ((unsigned long)elts[x]);
      ++x;
    }
  return hash;
}


