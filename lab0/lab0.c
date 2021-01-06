/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <limits.h>

void printUsageAndExit(char* exec) {
    printf("Usage: %s [--input INFILE] [--output OUTFILE] [--segfault] [--catch]\n", exec);
    _exit(1);
}

void forceSegFault() {
    char* ptr = NULL;
    *ptr = 'a';
}

void handleSegFault() {
    fprintf(stderr, "Caught and received segfault.\n");
    _exit(4);
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
                // Unrecognized option
                printUsageAndExit(argv[0]);
        }

    }

    // printf("fin: %s\tfout: %s\tsegflag: %d\tcatchflag: %d\n", fin, fout, segflag, catchflag);

    if(catchflag) signal(SIGSEGV, handleSegFault);
    if(segflag) forceSegFault();

    int BUFF_SIZE = 512;

    char buffer[BUFF_SIZE];
    size_t size_read;
    if(fin) {
        // Read from input file
        int ifd_rd = open(fin, O_RDONLY);

        if(ifd_rd >= 0) {
            close(0);
            size_read = read(ifd_rd, buffer, BUFF_SIZE);
            close(ifd_rd);
        } else {
            // Cannot read from given input file
            fprintf(stderr, "Unable to open input file %s: %s\n", fin, strerror(errno));
            _exit(2);
        }
    } else {
        // Read from standard input
        size_read = read(0, buffer, BUFF_SIZE);
        close(0);
    }

    if(fout) {
        // Write to output file
        int ifd_wr = open(fout, O_WRONLY);

        if(ifd_wr >= 0) {
            // Able to write to given output file
            close(1);
            write(ifd_wr, buffer, size_read);
            close(ifd_wr);
        } else {
            // Attempt to create new output file
            ifd_wr = creat(fout, 0666);

            if(ifd_wr >= 0) {
                close(1);
                write(ifd_wr, buffer, size_read);
                close(ifd_wr);
            } else {
                // Cannot create given output file
                fprintf(stderr, "Unable to create output file %s: %s\n", fout, strerror(errno));
                _exit(3);
            }
        }
    } else {
        // Write to standard output
        write(1, buffer, size_read);
        close(1);
    }

    _exit(0);
}