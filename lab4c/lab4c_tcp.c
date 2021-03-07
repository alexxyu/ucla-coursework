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
#include <netdb.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>

#ifdef DUMMY
#define MRAA_SUCCESS 0
typedef int* mraa_aio_context;

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
int print_celsius, should_report;
int sample_interval, fd_log, sockfd;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s port --id=id --host=hostname --log=logfile [--scale={C|F}] [--period=period]\n", exec);
    exit(1);
}

void cleanup_io_on_shutdown() {
    fprintf(stderr, "Shutting down...\n");
    shutdown(sockfd, SHUT_RDWR);
    if(close(fd_log) < 0) {
        fprintf(stderr, "Unable to close log file %s: %s\n", logfile, strerror(errno));
        exit(1);
    }
}

void write_to_server_and_log(char* msg, int nbytes) {
    if(write(sockfd, msg, nbytes) < 0) {
        fprintf(stderr, "Error writing message to server: %s\n", strerror(errno));
        exit(1);
    }
    if(write(fd_log, msg, nbytes) < 0) {
        fprintf(stderr, "Error writing message to log: %s\n", strerror(errno));
        exit(1);
    }
}

void perform_shutdown() {
    time_t curr_t;
    struct tm* tm_struct;
    char buffer[BUFFER_SIZE];

    time(&curr_t);
    tm_struct = localtime(&curr_t);
    int nbytes = sprintf(buffer, "%.2d:%.2d:%.2d SHUTDOWN\n", tm_struct->tm_hour, tm_struct->tm_min, tm_struct->tm_sec);

    write_to_server_and_log(buffer, nbytes);

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

    dprintf(fd_log, "%s\n", command_str);

    if(should_shutdown) {
        perform_shutdown();
    }
}

void parse_commands() {
    int read_size, i;
    
    if( poll(pollfds, 1, POLL_INTERVAL) > 0 && (pollfds[0].revents & POLLIN) ) {
        read_size = read(sockfd, commands_buffer, BUFFER_SIZE);
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

void connect_to_server(char* host, int port) {
    struct sockaddr_in serv_addr;
    struct hostent* server;

    // Create socket
    sockfd = socket(AF_INET /*protocol domain*/, SOCK_STREAM /*type*/, 0 /*protocol*/);
    if(sockfd < 0) {
        fprintf(stderr, "Error creating socket: %s\n", strerror(errno));
        exit(1);
    }

    // Initialize server address struct
    server = gethostbyname(host);
    if(server == NULL) {
        fprintf(stderr, "Error getting host by name\n");
        exit(1);
    }

    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(port);

    // Connect to server
    if(connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
        fprintf(stderr, "Error connecting to server: %s\n", strerror(errno));
        exit(1);
    }
}

int main(int argc, char* argv[]) {
    logfile = NULL;
    should_report = 1;
    print_celsius = DEFAULT_IN_CELSIUS;
    sample_interval = DEFAULT_SAMPLE_INTERVAL;

    const struct option long_options[] = {
        {"id", required_argument, 0, 'i'},
        {"log", required_argument, 0, 'l'},
        {"host", required_argument, 0, 'h'},
        {"scale", required_argument, 0, 's'},
        {"period", required_argument, 0, 'p'},
        {0, 0, 0, 0}
    };

    int c, opt_index;
    char* id = NULL;
    char* host = NULL;
    char* scale_str = NULL;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {
        switch(c) {
            case 'i':
                id = optarg;
                break;
            case 'h':
                host = optarg;
                break;
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

    // Handle extra arguments (there must be exactly one more argument for the port number)
    if(argc - optind != 1) {
        print_usage_and_exit(argv[0]);
    }

    // Validate and parse provided arguments
    if(logfile == NULL || host == NULL || id == NULL) {
        print_usage_and_exit(argv[0]);
    }
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

    // Validate ID string
    char* str_ptr = id;
    for(str_ptr = id; *str_ptr != '\0'; str_ptr++) {
        if(*str_ptr < '0' || *str_ptr > '9') {
            fprintf(stderr, "Please enter a valid 9-digit ID\n");
            print_usage_and_exit(argv[0]);
        }
    }
    if(str_ptr - id != 9) {
        fprintf(stderr, "Please enter a valid 9-digit ID\n");
        print_usage_and_exit(argv[0]);
    }

    // Parse port number and create log
    char* port_arg = argv[optind];
    int port = (int) strtol(port_arg, NULL, 10);
    fd_log = creat(logfile, 0666);
    if(fd_log < 0) {
        fprintf(stderr, "Unable to create log file %s: %s\n", logfile, strerror(errno));
        exit(1);
    }

    // Initialize AIO
    mraa_aio_context aio = mraa_aio_init(TEMP_SENSOR_PORT);
    if (aio == NULL) {
        if(aio == NULL) fprintf(stderr, "Failed to initialize AIO\n");
        mraa_deinit();
        exit(1);
    }

    // Connect to server and set up polling
    connect_to_server(host, port);
    pollfds[0].fd = sockfd;
    pollfds[0].events = POLLIN;

    fprintf(stderr, "Connected to server\n");

    // Send initial ID message to server
    int nbytes;
    char buffer[BUFFER_SIZE];
    nbytes = sprintf(buffer, "ID=%s\n", id);
    write_to_server_and_log(buffer, nbytes);

    atexit(cleanup_io_on_shutdown);

    // Setup for main loop reports
    struct tm* tm_struct;
    time_t time_of_report, curr_time;
    time_t elapsed_since_last_report = 0;

    // Main loop that generates reports
    time(&time_of_report);
    while(run_flag) {
        if(should_report && elapsed_since_last_report >= sample_interval) {
            time(&time_of_report);
            tm_struct = localtime(&time_of_report);
            int temp_reading = mraa_aio_read(aio);
            nbytes = sprintf(buffer, "%.2d:%.2d:%.2d %.1f\n", tm_struct->tm_hour, tm_struct->tm_min, tm_struct->tm_sec, 
                                                              calculate_temp(temp_reading, print_celsius));

            write_to_server_and_log(buffer, nbytes);

            elapsed_since_last_report = 0;
        }

        parse_commands();
        
        time(&curr_time);
        elapsed_since_last_report = curr_time - time_of_report;
    }

    // Close AIO
    int aio_status = mraa_aio_close(aio);
    if (aio_status != MRAA_SUCCESS) {
        exit(1);
    }

    exit(0);
}