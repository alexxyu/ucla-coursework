#line 141 "bits.c"
int bitParity(int x) {
#line 147
  int parity=  x ^( x >> 16);
  parity = parity ^( parity >> 8);
  parity = parity ^( parity >> 4);
  parity = parity ^( parity >> 2);
  parity = parity ^( parity >> 1);
  return parity & 1;
}
#line 162
int rotateRight(int x, int n) {
#line 168
  int oppN=  32 +( ~n + 1);
  int left=  x >> n & ~(~0 << oppN);
  int right=(  x << oppN);
  return left | right;
}
#line 182
int byteSwap(int x, int n, int m) {
#line 189
  int mBits=  m << 3;
  int nBits=  n << 3;

  int r1=(  x >> mBits) & 0xFF;
  int r2=(  x >> nBits) & 0xFF;
  x ^=( r1 << mBits);
  x ^=( r2 << nBits);
  r1 <<= nBits;
  r2 <<= mBits;

  return x ^ r1 ^ r2;
}
#line 209
int fitsShort(int x) {
#line 219
  int sign=(  x >> 31) & 1;
  return !((x >> 15) ^(( sign << 31) >> 31));
}
#line 229
int bitAnd(int x, int y) {
#line 234
  return ~(~x | ~y);
}
#line 244
int subOK(int x, int y) {
#line 251
  int diff=  x +( ~y + 1);
  int diffSign=(  diff >> 31) & 1;
  int xSign=(  x >> 31) & 1;
  int ySign=(  y >> 31) & 1;
  return !((xSign &( !ySign) &( !diffSign)) |(( !xSign) & ySign & diffSign));
}
#line 264
int isGreater(int x, int y) {
#line 273
  int xSign=(  x >> 31) & 1;
  int ySign=(  y >> 31) & 1;

  int negXposY=  xSign&(!ySign);
  int posXnegY=(  !xSign)&ySign;
  int diff=  x +( ~y+1);
  int diffSign=(  diff >> 31) & 1;

  return (!negXposY) &( posXnegY |(( !posXnegY) & !diffSign)) & !!diff;
}
#line 292
int fitsBits(int x, int n) {
#line 302
  int sign=(  x >> 31) & 1;
  int negativeOne=(  ~1+1);
  return !( x >>( n + negativeOne) ^(( sign << 31) >> 31));
}
#line 313
int negate(int x) {
#line 317
  return ~x + 1;
}
#line 326
int isTmax(int x) {
#line 331
  return !((x+1)^(!(x+1)) ^ ~x);
}
