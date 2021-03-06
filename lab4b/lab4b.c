/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <math.h>
#include <time.h>
#include <poll.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>

#ifdef DUMMY
#define MRAA_SUCCESS 0
#define MRAA_GPIO_IN 0
#define MRAA_GPIO_EDGE_RISING 0

typedef int* mraa_aio_context;
typedef int* mraa_gpio_context;

void mraa_deinit() {
}

/*
Define AIO functions
*/
mraa_aio_context mraa_aio_init(int p) {
    int* context = (int*) malloc(sizeof(int));
    *context = p;
    return context;
}
int mraa_aio_read(mraa_aio_context c) {
    return 650 + *c;
}
int mraa_aio_close(mraa_aio_context c) {
    free(c);
    return MRAA_SUCCESS;
}

/*
Define GPIO functions
*/
mraa_gpio_context mraa_gpio_init(int p) {
    int* context = (int*) malloc(sizeof(int));
    *context = p;
    return context;
}
void mraa_gpio_dir(mraa_gpio_context c, int d) {
    *c = *c+d-d;
}
void mraa_gpio_isr(mraa_gpio_context c, int edge, void* fptr, void* args) {
    if(fptr == NULL || args == NULL)
        return;
    *c = *c+edge;
}
int mraa_gpio_read(mraa_gpio_context c) {
    return 650 + *c;
}
int mraa_gpio_close(mraa_gpio_context c) {
    free(c);
    return MRAA_SUCCESS;
}
#else
#include <mraa.h>
#endif

#define B 4275
#define R0 100000
#define BUTTON_PORT 60
#define TEMP_SENSOR_PORT 1

#define BUFFER_SIZE 256
#define POLL_INTERVAL 50
#define DEFAULT_IN_CELSIUS 0
#define DEFAULT_SAMPLE_INTERVAL 1

struct pollfd pollfds[1];
int curr_command_length = 0;
char curr_command_buffer[BUFFER_SIZE];
char commands_buffer[BUFFER_SIZE];

sig_atomic_t volatile run_flag = 1;

char* logfile;
int sample_interval, fd_log;
int print_celsius, should_report;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s [--log=<LOGFILE>] [--scale={C|F}] [--period=<PERIOD>]\n", exec);
    exit(1);
}

void handle_interrupt(int sig) {
    if(sig == SIGINT)
        run_flag = 0;
}

void shutdown() {
    time_t curr_t;
    struct tm* tm_struct;
    char buffer[BUFFER_SIZE];

    time(&curr_t);
    tm_struct = localtime(&curr_t);
    int nbytes = sprintf(buffer, "%.2d:%.2d:%.2d SHUTDOWN\n", tm_struct->tm_hour, tm_struct->tm_min, tm_struct->tm_sec);
    fprintf(stdout, "%s", buffer);
    if(logfile) {
        write(fd_log, buffer, nbytes);
    }

    run_flag = 0;
}

void handle_command(char* command_str, int length) {
    int should_shutdown = 0;

    // Check whether command matches any correct format and take appropriate action
    if(strncmp(command_str, "SCALE=F", length) == 0) {
        print_celsius = 0;
    } else if(strncmp(command_str, "SCALE=C", length) == 0) {
        print_celsius = 1;
    } else if(strncmp(command_str, "PERIOD=", strlen("PERIOD=")) == 0) {
        int parsed_val = (int) strtol(command_str+strlen("PERIOD="), NULL, 10);
        if(parsed_val > 0) {
            // Only set sample interval if value is valid
            sample_interval = parsed_val;
        }
    } else if(strncmp(command_str, "STOP", length) == 0) {
        should_report = 0;
    } else if(strncmp(command_str, "START", length) == 0) {
        should_report = 1;
    } else if(strncmp(command_str, "LOG ", strlen("LOG ")) == 0) {
        // Do nothing special: LOG is a valid command but simply prints to log file
    } else if(strncmp(command_str, "OFF", length) == 0) {
        should_shutdown = 1;
    }

    if(logfile) {
        dprintf(fd_log, "%s\n", command_str);
    }
    if(should_shutdown) {
        shutdown();
    }
}

void parse_commands() {
    int read_size, i;
    
    if( poll(pollfds, 1, POLL_INTERVAL) > 0 && (pollfds[0].revents & POLLIN) ) {
        read_size = read(STDIN_FILENO, commands_buffer, BUFFER_SIZE);
        for(i=0; i<read_size; i++) {
            char c = commands_buffer[i];

            // Newline character indicates that the full command has been sent
            if(c == '\n') {
                curr_command_buffer[curr_command_length] = '\0';
                handle_command(curr_command_buffer, curr_command_length);

                // Reset current command buffer
                memset(curr_command_buffer, 0, BUFFER_SIZE);
                curr_command_length = 0;
            } else {
                if(curr_command_length || (c != ' ' && c != '\t')) {
                    // Copy character into buffer but skip opening spaces/tabs
                    curr_command_buffer[curr_command_length++] = c;
                }
            }
        }
    }
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
    logfile = NULL;
    should_report = 1;
    print_celsius = DEFAULT_IN_CELSIUS;
    sample_interval = DEFAULT_SAMPLE_INTERVAL;

    pollfds[0].fd = STDIN_FILENO;
    pollfds[0].events = POLLIN;

    const struct option long_options[] = {
        {"log", required_argument, 0, 'l'},
        {"scale", required_argument, 0, 's'},
        {"period", required_argument, 0, 'p'},
        {0, 0, 0, 0}
    };

    int c, opt_index;
    char* scale_str = NULL;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {
        switch(c) {
            case 'l':
                logfile = optarg;
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

    // Check validity and parse provided arguments
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
    if(logfile) {
        fd_log = creat(logfile, 0666);
        if(fd_log < 0) {
            fprintf(stderr, "Unable to create log file %s: %s\n", logfile, strerror(errno));
            exit(1);
        }
    }

    // Initialize AIO and GPIO
    mraa_aio_context aio = mraa_aio_init(TEMP_SENSOR_PORT);
    mraa_gpio_context gpio = mraa_gpio_init(BUTTON_PORT);
    if (aio == NULL || gpio == NULL) {
        if(aio == NULL) fprintf(stderr, "Failed to initialize AIO\n");
        if(gpio == NULL) fprintf(stderr, "Failed to initialize GPIO\n");
        mraa_deinit();
        exit(1);
    }
    mraa_gpio_dir(gpio, MRAA_GPIO_IN);
    mraa_gpio_isr(gpio, MRAA_GPIO_EDGE_RISING, &shutdown, NULL);
    signal(SIGINT, handle_interrupt);

    struct tm* tm_struct;
    time_t time_of_report, curr_time;
    time_t elapsed_since_last_report = 0;

    time(&time_of_report);

    char buffer[BUFFER_SIZE];

    // Main loop that generates reports
    while(run_flag) {
        if(should_report && elapsed_since_last_report >= sample_interval) {
            time(&time_of_report);
            tm_struct = localtime(&time_of_report);
            int temp_reading = mraa_aio_read(aio);
            int nbytes = sprintf(buffer, "%.2d:%.2d:%.2d %.1f\n", tm_struct->tm_hour, tm_struct->tm_min, tm_struct->tm_sec, 
                                                                  calculate_temp(temp_reading, print_celsius));
            fprintf(stdout, "%s", buffer);
            if(logfile) {
                write(fd_log, buffer, nbytes);
            }

            elapsed_since_last_report = 0;
        }

        parse_commands();
        
        time(&curr_time);
        elapsed_since_last_report = curr_time - time_of_report;
    }

    // Close AIO and GPIO
    int aio_status = mraa_aio_close(aio);
    int gpio_status = mraa_gpio_close(gpio);
    if (aio_status != MRAA_SUCCESS || gpio_status != MRAA_SUCCESS) {
        exit(1);
    }

    if(logfile) {
        if(close(fd_log) < 0) {
            fprintf(stderr, "Unable to close log file %s: %s\n", logfile, strerror(errno));
            exit(1);
        }
    }

    exit(0);
}