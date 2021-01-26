/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <getopt.h>
#include <pthread.h>

long long* ptr;
long n_threads, n_iters;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s --port=PORTNO [--log=FILENAME] [--compress]\n", exec);
    exit(1);
}

void add(long long *pointer, long long value) {
    long long sum = *pointer + value;
    *pointer = sum;
}

void *run() {
    for(long i=0; i<n_iters; i++) {
        add(ptr, 1);
    }
    for(long i=0; i<n_iters; i++) {
        add(ptr, -1);
    }
    pthread_exit(NULL);
}

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"threads", required_argument, 0, 't'},
        {"iterations", required_argument, 0, 'i'},
        {0, 0, 0, 0}
    };
    
    n_threads = 1;
    n_iters = 1;

    // Process options and arguments
    int c, opt_index;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {
        switch(c) {
            case 't':
                n_threads = strtol(optarg, NULL, 10);
                break;
            case 'i':
                n_iters = strtol(optarg, NULL, 10);
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
    }

    // Handle errors regarding provided arguments
    if(optind < argc) {
        print_usage_and_exit(argv[0]);
    }
    if(n_threads <= 0) {
        fprintf(stderr, "Please enter a valid number of threads\n");
        print_usage_and_exit(argv[0]);
    }
    if(n_iters <= 0) {
        fprintf(stderr, "Please enter a valid number of iterations\n");
        print_usage_and_exit(argv[0]);
    }

    long long counter = 0;
    ptr = &counter;

    pthread_t threads[256];
    struct timespec start_tp, end_tp;

    // Get high resolution start time of run
    if(clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_tp) < 0) {
        fprintf(stderr, "Error getting clock time: %s\n", strerror(errno));
        exit(1);
    }

    // Create specified number of threads and run
    for(long i=0; i<n_threads; i++) {
        if(pthread_create(&threads[i], NULL, run, (void*) i)) {
            fprintf(stderr, "Error creating thread: %s\n", strerror(errno));
            exit(1);
        }
    }
    for(long i=0; i<n_threads; i++) {
        if(pthread_join(threads[i], NULL)) {
            fprintf(stderr, "Error joining threads: %s\n", strerror(errno));
            exit(1);
        }
    }

    // Get high resolution end time of run
    if(clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_tp) < 0) {
        fprintf(stderr, "Error getting clock time: %s\n", strerror(errno));
        exit(1);
    }

    // Print out CSV record of results
    long n_operations = n_threads * n_iters * 2;
    long run_time = end_tp.tv_nsec - start_tp.tv_nsec;
    long avg_time = run_time / n_operations;
    fprintf(stdout, "%s,%ld,%ld,%ld,%ld,%ld,%lld\n", "add-none", n_threads, n_iters, n_operations, run_time, avg_time, *ptr);

    pthread_exit(NULL);

}
