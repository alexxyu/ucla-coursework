/* 
 * CS:APP Data Lab 
 *
 * Alex Yu - 105295708 
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
 : or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/*
 * bitParity - returns 1 if x contains an odd number of 0's
 *   Examples: bitParity(5) = 0, bitParity(7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int bitParity(int x) {
  /*
   * Whether the number of 0's is even or odd can be found by using a 
   *   folding approach. This is done by repeatedly xor'ing together
   *   both "halves" of the integer from 32 bits down to 1 bit.
   */
  int parity = x ^ (x >> 16);         // 16-bit parity
  parity = parity ^ (parity >> 8);    // 8-bit parity
  parity = parity ^ (parity >> 4);    // 4-bit parity
  parity = parity ^ (parity >> 2);    // 2-bit parity
  parity = parity ^ (parity >> 1);    // 1-bit parity
  return parity & 1;
}
/* 
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x18765432
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3 
 */
int rotateRight(int x, int n) {
  /* 
   * This function rotates x by creating a mask that shifts left 
   *   and a mask that shifts right. The result is the masks or'ed
   *   together.
   */
  int oppN = 32 + (~n + 1);
  int left = x >> n & ~(~0 << oppN);
  int right = (x << oppN);
  return left | right;
}
/* 
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
  /* 
   * This function swaps bytes by using mask to extract each byte, 
   *   left shifting each byte into their appropriately swapped position,
   *   and xor'ing to get the resulting integer. 
   */

  int mBits = m << 3;
  int nBits = n << 3;
  
  int r1 = (x >> mBits) & 0xFF;  
  int r2 = (x >> nBits) & 0xFF;
  x ^= (r1 << mBits);
  x ^= (r2 << nBits);
  r1 <<= nBits;
  r2 <<= mBits;

  return x ^ r1 ^ r2;  
}
/* 
 * fitsShort - return 1 if x can be represented as a 
 *   16-bit, two's complement integer.
 *   Examples: fitsShort(33000) = 0, fitsShort(-32768) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int fitsShort(int x) {
  /*
   * x can be represented as a 16-bit, two's comp int if all bits 
   *   past the first 15 bits match its signed bit. 
   * 
   * The following must be true for x to be representable in 16 bits:
   *   For x > 0, x's max is when the first 15 bits are 1 and the rest are 0. 
   *   For x < 0, x's min is when the first 15 bits are 0 and the rest are 1.
   */

  int sign = (x >> 31) & 1;
  return !((x >> 15) ^ ((sign << 31) >> 31));
}
/* 
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y) {
  /*
   * Using De Morgan's Laws, x&y = ~(~x | ~y) when distributing the  
   *   ~ bit operation.
   */
  return ~(~x | ~y);
}
/* 
 * subOK - Determine if can compute x-y without overflow
 *   Example: subOK(0x80000000,0x80000000) = 1,
 *            subOK(0x80000000,0x70000000) = 0, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int subOK(int x, int y) {
  /*
   * x-y CANNOT be computed without overflow under these two cases:
   *   1. x<0, y>0, and x-y > 0 (overflow since x-y should be negative)
   *   2. x>0, y<0, and x-y < 0 (overflow since x-y should be positive)
   */

  int diff = x + (~y + 1);
  int diffSign = (diff >> 31) & 1;
  int xSign = (x >> 31) & 1;
  int ySign = (y >> 31) & 1;
  return !((xSign & (!ySign) & (!diffSign)) | ((!xSign) & ySign & diffSign));  
}
/* 
 * isGreater - if x > y  then return 1, else return 0 
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
  /*
   * x > y for three cases:
   *   1. x is positive and y is negative
   *   2. x and y are both positive, x - y > 0 
   *   3. x and y are both negative, x - y > 0
   * Important note: cases 2 and 3 are safe from overflow subtraction.
   */

  int xSign = (x >> 31) & 1;
  int ySign = (y >> 31) & 1;
  
  int negXposY = xSign & (!ySign);
  int posXnegY = (!xSign) & ySign;
  int diff = x + (~y+1);
  int diffSign = (diff >> 31) & 1;

  return (!negXposY) & (posXnegY | ((!posXnegY) & !diffSign)) & !!diff;
}
/* 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
  /*
   * The logic is nearly identical to that of the fitsShort function.
   * x can be represented as a n-bit, two's comp int if all bits 
   *   past the first n-1 bits match its signed bit. 
   * 
   * The following must be true for x to be representable in n bits:
   *   For x > 0, x's max is when the first n-1 bits are 1 and the rest are 0. 
   *   For x < 0, x's min is when the first n-1 bits are 0 and the rest are 1.
   */
  int sign = (x >> 31) & 1;
  int negativeOne = (~1+1);
  return !((x >> (n + negativeOne) ^ ((sign << 31) >> 31)));
}
/* 
 * negate - return -x 
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {
  /*
   * By definition, for a two's complement integer, -x = ~x+1. 
   */
  return ~x + 1;
}
/*
 * isTmax - returns 1 if x is the maximum, two's complement number,
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmax(int x) {
  /*
   * If x is Tmax, then x+1 = Tmin by overflow. This function essentially 
   *   checks that x+1 is Tmin.
   */
  return !((x+1)^(!(x+1)) ^ ~x);
}
