#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include "output.h"

#define KB_TO_B 1024

static char* buffer;
static int bufferctr;
static int blocksize;

/* Initialize writing by block with specified block size (in KiB) */
void writer_init (int bsize)
{
  blocksize = bsize * KB_TO_B;
  buffer = (char*) malloc(blocksize);
  if (buffer == NULL)
  {
    fprintf(stderr, "malloc error\n");
    abort();
  }
  bufferctr = 0;
}

/* Write byte by byte to standard output */
bool writebytes (unsigned long long x, int nbytes)
{
  do
  {
    if (putchar (x) < 0)
	    return false;
    x >>= CHAR_BIT;
    nbytes--;
  }
  while (0 < nbytes);

  return true;
}

/* Write block by block to standard output */
bool writeblocks (unsigned long long x, int nbytes)
{
  do
  {
    buffer[bufferctr++] = x & 0xff;
    x >>= CHAR_BIT;
    nbytes--;

    if (bufferctr >= blocksize)
    {
      bufferctr = 0;
      if (write(1, buffer, blocksize) != blocksize)
        return false;
    }
  }
  while (0 < nbytes);

  return true;
}

bool write_remaining (void)
{
  int ret = write(1, buffer, bufferctr);
  bufferctr = 0;

  return ret >= 0;
}

/* Write remaining contents of buffer, free allocated memory */
void writer_fini (void)
{
  free(buffer);
}

