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
#include <signal.h>
#include <pthread.h>
#include "SortedList.h"

#define DEFAULT_THREADS 1
#define DEFAULT_ITERS 1

#define NO_SYNC 0
#define M_SYNC 1
#define S_SYNC 2

char** keys;
SortedList_t list, *pool;

int opt_sync, opt_yield;
long n_threads, n_iters, spinlock;
pthread_mutex_t mutex;

static void handle_segfault() {
    fprintf(stderr, "Error: received and caught segfault during execution\n");
    exit(2);
}

void cleanup() {
    if(opt_sync == M_SYNC) pthread_mutex_destroy(&mutex);

    int n_elements = n_iters * n_threads;
    for(int i=0; i<n_elements; i++) free(keys[i]);
    free(keys);
    free(pool);
}

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s [-iterations=<ITERS>] [--threads=<THREADS>] [--yield=[idl]] [--sync={m|s}}]\n", exec);
    exit(1);
}

void set_test_name(char* name, int size) {
    sprintf(name, "%s", "list-");
    char tmp[20];

    // Format yield option(s) in test name
    memcpy(tmp, name, size);
    if(opt_yield == 0) {
        sprintf(name, "%snone", tmp);
    }
    memcpy(tmp, name, size);
    if(opt_yield & INSERT_YIELD) {
        sprintf(name, "%si", tmp);
    }
    memcpy(tmp, name, size);
    if(opt_yield & DELETE_YIELD) {
        sprintf(name, "%sd", tmp);
    }
    memcpy(tmp, name, size);
    if(opt_yield & LOOKUP_YIELD) {
        sprintf(name, "%sl", tmp);
    }

    // Format sync option in test name
    memcpy(tmp, name, size);
    switch(opt_sync) {
        case M_SYNC:
            sprintf(name, "%s-m", tmp);
            break;
        case S_SYNC:
            sprintf(name, "%s-s", tmp);
            break;
        default:
            sprintf(name, "%s-none", tmp);
            break;
    }
}

void *run(void *threadid) {
    long tid = (long) threadid;
    long offset = tid * n_iters;
    for(long i=0; i<n_iters; i++) {
        // Lock as needed and insert into list
        if(opt_sync == M_SYNC) {
            pthread_mutex_lock(&mutex);
        } else if(opt_sync == S_SYNC) {
            while(__sync_lock_test_and_set(&spinlock, 1) == 1);
        }

        SortedList_insert(&list, &pool[i+offset]);

        if(opt_sync == M_SYNC) {
            pthread_mutex_unlock(&mutex);
        } else if(opt_sync == S_SYNC) {
            __sync_lock_release(&spinlock);
        }
    }

    // Lock as needed and get length of list
    if(opt_sync == M_SYNC) {
        pthread_mutex_lock(&mutex);
    } else if(opt_sync == S_SYNC) {
        while(__sync_lock_test_and_set(&spinlock, 1) == 1);
    }

    SortedList_length(&list);

    if(opt_sync == M_SYNC) {
        pthread_mutex_unlock(&mutex);
    } else if(opt_sync == S_SYNC) {
        __sync_lock_release(&spinlock);
    }

    for(long i=0; i<n_iters; i++) {
        // Lock as needed, lookup element in list, and delete
        if(opt_sync == M_SYNC) {
            pthread_mutex_lock(&mutex);
        } else if(opt_sync == S_SYNC) {
            while(__sync_lock_test_and_set(&spinlock, 1) == 1);
        }

        SortedListElement_t *elem = SortedList_lookup(&list, pool[i+offset].key);
        if(elem == NULL) {
            fprintf(stderr, "Error: element that should have been in list was not found\n");
            exit(2);
        } else if(SortedList_delete(elem) == 1) {
            fprintf(stderr, "Error: corrupted pointer(s) in list found\n");
            exit(2);
        }

        if(opt_sync == M_SYNC) {
            pthread_mutex_unlock(&mutex);
        } else if(opt_sync == S_SYNC) {
            __sync_lock_release(&spinlock);
        }
    }
    return NULL;
}

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"threads", required_argument, 0, 't'},
        {"iterations", required_argument, 0, 'i'},
        {"yield", required_argument, 0, 'y'},
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
    char *sync_str = NULL, *yield_str = NULL;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {
        switch(c) {
            case 't':
                n_threads = strtol(optarg, NULL, 10);
                break;
            case 'i':
                n_iters = strtol(optarg, NULL, 10);
                break;
            case 'y':
                yield_str = optarg;
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
            default:
                fprintf(stderr, "Please provide a valid synchronization option\n");
                print_usage_and_exit(argv[0]);
        }
    }
    if(yield_str) {
        int yield_len = (int) strlen(yield_str);
        for(int i=0; i<yield_len; i++) {
            switch(yield_str[i]) {
                case 'i':
                    opt_yield |= INSERT_YIELD;
                    break;
                case 'd':
                    opt_yield |= DELETE_YIELD;
                    break;
                case 'l':
                    opt_yield |= LOOKUP_YIELD;
                    break;
                default:
                    fprintf(stderr, "Please provide a valid yield option\n");
                    print_usage_and_exit(argv[0]);
            }
        }
    }

    struct sigaction sa;
    sa.sa_handler = handle_segfault;
    sigaction(SIGSEGV, &sa, NULL);

    // Initialize list and generate pool of elements for threads
    list.next = &list;
    list.prev = &list;
    list.key = NULL;

    time_t t;
    srand((unsigned) time(&t));

    long n_elements = n_iters * n_threads;
    atexit(cleanup);
    if((pool = malloc(sizeof(SortedListElement_t) * n_elements)) == NULL) {
        fprintf(stderr, "Error allocating memory: %s", strerror(errno));
        exit(1);
    }
    if((keys = malloc(n_elements * sizeof(char*))) == NULL) {
        fprintf(stderr, "Error allocating memory: %s", strerror(errno));
        exit(1);
    }
    for(long i=0; i<n_elements; i++) {
        if((keys[i] = malloc(sizeof(char))) == NULL) {
            fprintf(stderr, "Error allocating memory: %s", strerror(errno));
            exit(1);
        }
        *keys[i] = 'A' + (rand() % 26);
        SortedListElement_t elem = {NULL, NULL, keys[i]};
        pool[i] = elem;
    }

    pthread_t threads[256];
    struct timespec start_tp, end_tp;

    // Get high resolution start time of run
    if(clock_gettime(CLOCK_MONOTONIC, &start_tp) < 0) {
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
    if(clock_gettime(CLOCK_MONOTONIC, &end_tp) < 0) {
        fprintf(stderr, "Error getting clock time: %s\n", strerror(errno));
        exit(1);
    }
    
    int length;
    if((length = SortedList_length(&list)) != 0) {
        fprintf(stderr, "Error: final length of list is not 0 (returned %d)\n", length);
        exit(2);
    }

    // Print out CSV record of results
    long long n_operations = (long long) n_threads * n_iters * 3;
    long long run_time = 1000000000L * (end_tp.tv_sec - start_tp.tv_sec) + (end_tp.tv_nsec - start_tp.tv_nsec);
    long long avg_time = run_time / n_operations;

    char name[20];
    set_test_name(name, sizeof(name));
    fprintf(stdout, "%s,%ld,%ld,%d,%lld,%lld,%lld\n", name, n_threads, n_iters, 1, n_operations, 
                                                      run_time, avg_time);

    exit(0);

}
