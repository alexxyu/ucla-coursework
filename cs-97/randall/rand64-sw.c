#include <stdlib.h>
#include <time.h>
#include "rand64-sw.h"

/* Software implementation.  */

/* Initialize the software rand64 implementation.  */
void software_rand64_init (void)
{
  srand48(time(0));
}

/* Return a random value, using software operations.  */
unsigned long long software_rand64 (void)
{
  return mrand48();
}

/* Finalize the software rand64 implementation.  */
void software_rand64_fini (void)
{
}
