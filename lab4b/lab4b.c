#include <math.h>
#include <time.h>
#include <mraa.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>

#define B 4275
#define R0 100000
#define BUTTON_PORT 60
#define TEMP_SENSOR_PORT 1

#define BUFFER_SIZE 256
#define DEFAULT_IN_CELSIUS 0
#define DEFAULT_SAMPLE_INTERVAL 1

sig_atomic_t volatile run_flag = 1;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s [--log=<LOGFILE>] [--scale={C|F}] [--period=<PERIOD>]\n", exec);
    exit(1);
}

void handle_interrupt(int sig) {
    if(sig == SIGINT)
        run_flag = 0;
}

void handle_button_interrupt() {
    run_flag = 0;
}

float calculate_temp(float R, int in_celsius) {
    R = 1023.0/R-1.0;
    R = R0*R;

    float temp = 1.0/(log(R/R0) / B + 1/298.15) - 273.15;
    if(in_celsius) {
        return temp;
    }
    return temp * 9/5 + 32;
}

int main(int argc, char* argv[]) {
    int print_celsius = DEFAULT_IN_CELSIUS;
    int sample_interval = DEFAULT_SAMPLE_INTERVAL;

    const struct option long_options[] = {
        {"log", required_argument, 0, 'l'},
        {"scale", required_argument, 0, 's'},
        {"period", required_argument, 0, 'p'},
        {0, 0, 0, 0}
    };

    int c, opt_index;
    char* log = NULL, *scale_str = NULL;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {
        switch(c) {
            case 'l':
                log = optarg;
                break;
            case 's':
                scale_str = optarg;
                break;
            case 'p':
                sample_interval = (int) strtol(optarg, NULL, 10);
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
    }

    // Handle extra arguments
    if(optind < argc)
        print_usage_and_exit(argv[0]);

    // Check and parse provided arguments
    if(scale_str != NULL) {
        if(strlen(scale_str) != 1 || (scale_str[0] != 'C' && scale_str[0] != 'F')) {
            fprintf(stderr, "Please provide a valid scale\n");
            print_usage_and_exit(argv[0]);
        } else {
            print_celsius = (scale_str[0] == 'C');
        }
    }
    if(sample_interval <= 0) {
        fprintf(stderr, "Please enter a valid period\n");
        print_usage_and_exit(argv[0]);
    }
    int fd_log;
    if(log) {
        fd_log = creat(log, 0666);
        if(fd_log < 0) {
            fprintf(stderr, "Unable to create log file %s: %s\n", log, strerror(errno));
            exit(1);
        }
    }

    // Initialize AIO
    mraa_aio_context aio = mraa_aio_init(TEMP_SENSOR_PORT);
    mraa_gpio_context gpio = mraa_gpio_init(BUTTON_PORT);
    if (aio == NULL || gpio == NULL) {
        if(aio == NULL) fprintf(stderr, "Failed to initialize AIO\n");
        if(gpio == NULL) fprintf(stderr, "Failed to initialize GPIO\n");
        mraa_deinit();
        exit(1);
    }

    mraa_gpio_dir(gpio, MRAA_GPIO_IN);

    mraa_gpio_isr(gpio, MRAA_GPIO_EDGE_RISING, &handle_button_interrupt, NULL);
    signal(SIGINT, handle_interrupt);

    time_t curr_t;
    struct tm* tm_struct;
    char buffer[BUFFER_SIZE];

    while(run_flag) {
        time(&curr_t);
        tm_struct = localtime(&curr_t);
        int temp_reading = mraa_aio_read(aio);
        int nbytes = sprintf(buffer, "%.2d:%.2d:%.2d %.1f\n", tm_struct->tm_hour, tm_struct->tm_min, tm_struct->tm_sec, 
                                                              calculate_temp(temp_reading, print_celsius));
        fprintf(stdout, "%s", buffer);
        if(log) {
            write(fd_log, buffer, nbytes);
        }
        
        sleep(sample_interval);
    }

    // Close AIO
    int aio_status = mraa_aio_close(aio);
    int gpio_status = mraa_gpio_close(gpio);
    if (aio_status != MRAA_SUCCESS || gpio_status != MRAA_SUCCESS) {
        exit(1);
    }

    if(log) {
        if(close(fd_log) < 0) {
            fprintf(stderr, "Unable to close log file %s: %s\n", log, strerror(errno));
            exit(1);
        }
    }

    exit(0);
}