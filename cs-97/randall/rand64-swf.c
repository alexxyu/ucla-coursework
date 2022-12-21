#include <stdio.h>
#include <stdlib.h>
#include "rand64-swf.h"

/* Software implementation using file as source of random data.  */

/* Input stream containing random bytes.  */
static FILE *urandstream;

/* Initialize the software rand64 by file implementation.  */
void software_rand64_file_init (char* in)
{
  urandstream = fopen (in, "r");
  if (! urandstream)
    abort ();
}

/* Return a random value, reading from file source.  */
unsigned long long software_rand64_file (void)
{
  unsigned long long int x;
  if (fread (&x, sizeof x, 1, urandstream) != 1)
    abort ();
  return x;
}

/* Finalize the software rand64 by file implementation.  */
void software_rand64_file_fini (void)
{
  fclose (urandstream);
}
