/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <limits.h>

void printUsageAndExit(char* exec) {
    fprintf(stderr, "Usage: %s [--input INFILE] [--output OUTFILE] [--segfault] [--catch]\n", exec);
    exit(1);
}

void forceSegFault() {
    char* ptr = NULL;
    *ptr = 'a';
}

void handleSegFault() {
    fprintf(stderr, "Caught and received segfault.\n");
    exit(4);
}

int main(int argc, char *argv[]) {

    static struct option long_options[] = {
        {"input", required_argument, 0, 'i'},
        {"output", required_argument, 0, 'o'},
        {"segfault", no_argument, 0, 's'},
        {"catch", no_argument, 0, 'c'},
        {0, 0, 0, 0}
    };

    char* fin = NULL;
    char* fout = NULL;
    int segflag = 0, catchflag = 0;

    // Parse any given arguments
    int c, opt_index;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1) {
        
        switch(c) {
            case 'i':
                fin = optarg;
                break;
            case 'o':
                fout = optarg;
                break;
            case 's':
                segflag = 1;
                break;
            case 'c':
                catchflag = 1;
                break;
            default:
                // Handle unrecognized argument
                printUsageAndExit(argv[0]);
        }

    }

    // Handle extra arguments
    if(optind < argc)
        printUsageAndExit(argv[0]);

    // Handle input file redirection
    if(fin) {
        int ifd_rd = open(fin, O_RDONLY);

        if(ifd_rd >= 0) {
            close(0);
            dup(ifd_rd);
            close(ifd_rd);
        } else {
            // Cannot open given input file
            fprintf(stderr, "Unable to open input file %s: %s\n", fin, strerror(errno));
            exit(2);
        }
    } 

    // Handle output file redirection
    if(fout) {
        int ifd_wr = creat(fout, 0666);

        if(ifd_wr >= 0) {
            close(1);
            dup(ifd_wr);
            close(ifd_wr);
        } else {
            // Cannot create given output file
            fprintf(stderr, "Unable to create output file %s: %s\n", fout, strerror(errno));
            exit(3);
        }
    }

    if(catchflag) 
        signal(SIGSEGV, handleSegFault);

    if(segflag) 
        forceSegFault();

    int BUFF_SIZE = 512;
    char buffer[BUFF_SIZE];
    size_t size_read;
    
    // Continuously read from input and write to output until EOF
    while((size_read = read(0, buffer, BUFF_SIZE)) > 0) {
        if( write(1, buffer, size_read) < 0 ) {
            fprintf(stderr, "Error writing to output: %s\n", strerror(errno));
            exit(3);
        }
    }

    if(size_read < 0) {
        fprintf(stderr, "Error reading from input: %s\n", strerror(errno));
        exit(2);
    }

    close(0);
    close(1);
    exit(0);

}