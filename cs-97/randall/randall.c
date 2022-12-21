/* Generate N bytes of random output.  */

/* When generating output this program uses the x86-64 RDRAND
   instruction if available to generate random numbers, falling back
   on /dev/random and stdio otherwise.

   This program is not portable.  Compile it with gcc -mrdrnd for a
   x86-64 machine.

   Copyright 2015, 2017, 2020 Paul Eggert

   This program is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rand64-hw.h"
#include "rand64-sw.h"
#include "rand64-swf.h"
#include "output.h"
#include "options.h"

/* Main program, which outputs N bytes of random data.  */
int main (int argc, char **argv)
{
  int fileflg, blocksize = 0;
  long long nbytes;
  char* input;
  int errflg = processOptions(argc, argv, &nbytes, &input, &fileflg, &blocksize);
  
  /* Exit if error encountered from option processing  */
  if(errflg)
    return 1;

  /* If there's no work to do, don't worry about which library to use.  */
  if (nbytes == 0)
    return 0;

  /* Now that we know we have work to do, arrange to use the
     appropriate library.  */
  unsigned long long (*rand64) (void);
  void (*finalize) (void);

  if (strcmp(input, "rdrand") == 0) 
  {
    /* If hardware does not support random-number generation, throw error */
    if(!rdrand_supported ()) {
      fprintf(stderr, "Error: hardware does not support random-number generation\n");
      return 1;
    }

    hardware_rand64_init ();
    rand64 = hardware_rand64;
    finalize = hardware_rand64_fini;
  }
  else if(fileflg)
  {
    software_rand64_file_init (input);
    rand64 = software_rand64_file;
    finalize = software_rand64_file_fini;
  }
  else
  {
    software_rand64_init ();
    rand64 = software_rand64;
    finalize = software_rand64_fini;
    
  }

  int wordsize = sizeof rand64 ();
  int output_errno = 0;

  /* Set up write method (by byte vs. by block) */
  _Bool (*write) (unsigned long long x, int nbytes);
  if (blocksize == 0)
    write = writebytes;
  else 
  {
    writer_init(blocksize);
    write = writeblocks;
  }

  /* Generate and write numbers */
  do
  {  
    unsigned long long x = rand64 ();
    int outbytes = nbytes < wordsize ? nbytes : wordsize;
    if (!write (x, outbytes))
    {
      output_errno = errno;
      break;
    }
    nbytes -= outbytes;
  }
  while (0 < nbytes);

  if (blocksize != 0)
  {
    if (!write_remaining())
      output_errno = errno;
    writer_fini();
  }

  /* Finish up with error checking */
  if (fclose (stdout) != 0)
    output_errno = errno;

  if (output_errno)
  {
    errno = output_errno;
    perror ("output");
  }

  finalize ();
  return !!output_errno;
}
