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

#define DEFAULT_THREADS 1
#define DEFAULT_ITERS 1

#define NO_SYNC 0
#define M_SYNC 1
#define S_SYNC 2
#define C_SYNC 3

long long counter;
long n_threads, n_iters;
int opt_yield, opt_sync;

long spinlock;
pthread_mutex_t mutex;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s [-iterations=<ITERS>] [--threads=<THREADS>] [--yield] [--sync={m|s|c}}]\n", exec);
    exit(1);
}

char* get_test_name() {
    if(opt_yield) {
        if(opt_sync == M_SYNC) return "add-yield-m";
        if(opt_sync == S_SYNC) return "add-yield-s";
        if(opt_sync == C_SYNC) return "add-yield-c";

        return "add-yield-none";
    } 

    if(opt_sync == M_SYNC) return "add-m";
    if(opt_sync == S_SYNC) return "add-s";
    if(opt_sync == C_SYNC) return "add-c";

    return "add-none";
}

void add(long long *pointer, long long value) {
    long long sum = *pointer + value;
    if (opt_yield)
            sched_yield();
    *pointer = sum;
}

void atomic_add(long long *pointer, long long value) {
    long long prev, sum;
    do {
        prev = *pointer;
        sum = prev + value;
        if (opt_yield)
                sched_yield();
    } while(__sync_val_compare_and_swap(pointer, prev, sum) != prev);
}

void *run() {
    for(long i=0; i<n_iters; i++) {
        switch(opt_sync) {
            case NO_SYNC:
                add(&counter, 1);
                break;
            case M_SYNC:
                pthread_mutex_lock(&mutex);
                add(&counter, 1);
                pthread_mutex_unlock(&mutex);
                break;
            case S_SYNC:
                while(__sync_lock_test_and_set(&spinlock, 1) == 1);
                add(&counter, 1);
                __sync_lock_release(&spinlock);
                break;
            case C_SYNC:
                atomic_add(&counter, 1);
                break;
        }
    }

    for(long i=0; i<n_iters; i++) {
        switch(opt_sync) {
            case NO_SYNC:
                add(&counter, -1);
                break;
            case M_SYNC:
                pthread_mutex_lock(&mutex);
                add(&counter, -1);
                pthread_mutex_unlock(&mutex);
                break;
            case S_SYNC:
                while(__sync_lock_test_and_set(&spinlock, 1) == 1);
                add(&counter, -1);
                __sync_lock_release(&spinlock);
                break;
            case C_SYNC:
                atomic_add(&counter, -1);
                break;
        }
    }
    return NULL;
}

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"threads", required_argument, 0, 't'},
        {"iterations", required_argument, 0, 'i'},
        {"yield", no_argument, 0, 'y'},
        {"sync", required_argument, 0, 's'},
        {0, 0, 0, 0}
    };
    
    // Set default values
    n_threads = DEFAULT_THREADS;
    n_iters = DEFAULT_ITERS;
    opt_yield = 0;
    opt_sync = NO_SYNC;

    // Process options and arguments
    int c, opt_index;
    char* sync_str = NULL;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {
        switch(c) {
            case 't':
                n_threads = strtol(optarg, NULL, 10);
                break;
            case 'i':
                n_iters = strtol(optarg, NULL, 10);
                break;
            case 'y':
                opt_yield = 1;
                break;
            case 's':
                sync_str = optarg;
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
    if(sync_str) {
        if(strlen(sync_str) > 1) {
            fprintf(stderr, "Please provide a valid synchronization option\n");
            print_usage_and_exit(argv[0]);
        }

        switch(sync_str[0]) {
            case 'm':
                opt_sync = M_SYNC;
                pthread_mutex_init(&mutex, NULL);
                break;
            case 's':
                opt_sync = S_SYNC;
                spinlock = 0;
                break;
            case 'c':
                opt_sync = C_SYNC;
                break;
            default:
                fprintf(stderr, "Please provide a valid synchronization option\n");
                print_usage_and_exit(argv[0]);
        }
    }

    counter = 0;

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

    // Destroy mutex lock as necessary
    if(opt_sync == M_SYNC) pthread_mutex_destroy(&mutex);

    // Print out CSV record of results
    long n_operations = n_threads * n_iters * 2;
    long run_time = end_tp.tv_nsec - start_tp.tv_nsec;
    long avg_time = run_time / n_operations;

    fprintf(stdout, "%s,%ld,%ld,%ld,%ld,%ld,%lld\n", get_test_name(), n_threads, n_iters, n_operations, 
                                                     run_time, avg_time, counter);

    exit(0);

}
