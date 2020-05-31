#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "options.h"

int processOptions(int argc, char *argv[ ], long long* nbytes, 
                   char** input, int* fileflg, int *blocksize)
{
  char c;
  char* output = NULL;
  *input = NULL;

  /* Loop through args provided, throw errors as needed */
  while ((c = getopt(argc, argv, ":i:o:")) != -1) 
  {
    switch(c) 
    {
      case 'i':
        *input = optarg;
        break;
      case 'o':
        output = optarg;
        break;
      case ':':
        /* Missing operand in option */
        fprintf(stderr, "Option -%c requires an operand\n", optopt);
        return 1;
      case '?':
        /* Unrecognized option */
        fprintf(stderr, "Unrecognized option: '-%c'\n", optopt);
        return 1;
      default:
        abort();
    }
  }

  /* There must be exactly one non-optional argument specifying number of bytes */
  if (argc-optind != 1) 
  {
    fprintf(stderr, "%s usage: %s NBYTES [-i input] [-o output]\n", argv[0], argv[0]);
    return 1;
  }

  /* Process input option */
  char* tmp = *input;
  if (tmp == NULL)
    *input = "rdrand";
  else if (tmp[0] == '/')
  {
    /* File does not exist */
    if ( access( tmp, F_OK ) == -1)
    {
      fprintf(stderr, "%s does not exist\n", tmp);
      return 1;
    }
    *fileflg = 1;
  }
  else if (strcmp(tmp, "rdrand") == 0 || strcmp(tmp, "mrand48") == 0)
    *fileflg = 0;
  else
  {
    fprintf(stderr, "%s is not a valid input argument\n", tmp);
    return 1;
  }

  /* Process output option */
  if (output != NULL && strcmp(output, "stdio"))
  {
    char* endptr;
    int temp = strtol(output, &endptr, 10);

    /* error if provided string is NaN or a negative integer */
    if (endptr[0] != '\0' || temp < 0 || temp == 0)
    {
      fprintf(stderr, "%s is not a valid output argument\n", output);
      return 1;
    }
    *blocksize = temp;
  }

  /* Process nbyte argument */
  char* endptr;
  int errno = 0;
  *nbytes = strtoll(argv[optind], &endptr, 10);
  if(errno)
    perror (argv[optind]);
  else if(!(!*endptr && 0 <= *nbytes)) 
  {
    printf("NBYTES is invalid\n");
    return 1;
  }

  return 0;
}

