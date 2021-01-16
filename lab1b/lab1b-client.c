#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <getopt.h>
#include <poll.h>
#include <signal.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/types.h>
#include "constants.h"

struct pollfd* pollfds;
const int POLL_TIMEOUT = 0;
const short POLL_EVENTS = POLLIN | POLLHUP | POLLERR;

struct termios newmode;
struct termios currmode;

int cli_sockfd;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s --port=INT [--log=FILENAME] [--compress]\n", exec);
    exit(1);
}

void restore_terminal_mode() {
    if(tcsetattr(STDIN_FILENO, TCSANOW, &currmode) < 0) {
        fprintf(stderr, "Error while setting terminal mode attributes: %s", strerror(errno));
        exit(1);
    }
}

void set_terminal_mode() {

    if(tcgetattr(STDIN_FILENO, &newmode) < 0) {
        fprintf(stderr, "Error while getting terminal mode attributes: %s", strerror(errno));
        exit(1);
    }

    memcpy(&currmode, &newmode, sizeof(newmode));
    newmode.c_iflag = ISTRIP;	/* only lower 7 bits	*/
    newmode.c_oflag = 0;		/* no processing	*/
    newmode.c_lflag = 0;		/* no processing	*/
    
    if(tcsetattr(STDIN_FILENO, TCSANOW, &newmode) < 0) {
        fprintf(stderr, "Error while setting terminal mode attributes: %s", strerror(errno));
        exit(1);
    }
    atexit(restore_terminal_mode);

}

void write_char(int fd, const char ch) {

    if(write(fd, &ch, 1) < 0) {
        fprintf(stderr, "Write failed: %s\n", strerror(errno));
        exit(1);
    }

}

void process_input() {

    char buffer[BUFF_SIZE];
    int read_size;

    int exit_flag = 0;
    int n_ready, write_to_socket;
    while( !exit_flag && (n_ready = poll(pollfds, 2, POLL_TIMEOUT)) >= 0 ) {

        if(n_ready > 0) {
            if(pollfds[0].revents & POLLIN) {
                // Handle keyboard input
                read_size = read(pollfds[0].fd, buffer, BUFF_SIZE);
                if(read_size < 0) {
                    fprintf(stderr, "Read failed: %s\n", strerror(errno));
                    exit(1);
                }

                write_to_socket = 1;
            }

            if(pollfds[0].revents & POLLHUP || pollfds[0].revents & POLLERR) {
                fprintf(stderr, "Error polling from keyboard: %s", strerror(errno));
                exit(1);
            }

            if(pollfds[1].revents & POLLIN) {
                // Handle socket input
                read_size = read(pollfds[1].fd, buffer, BUFF_SIZE);
                if(read_size < 0) {
                    fprintf(stderr, "Read failed: %s\n", strerror(errno));
                    exit(1);
                }

                write_to_socket = 0;
            }

            if(pollfds[1].revents & POLLHUP || pollfds[1].revents & POLLERR) {
                // Receipt of polling error means no more output from shell
                exit_flag = 1;
            }

            // Write character to terminal stdout + shell if applicable
            for(int i=0; !exit_flag && i<read_size; i++) {
                char c = buffer[i];

                if(c == EOF_CODE) {
                    write_char(STDIN_FILENO, '^');
                    write_char(STDIN_FILENO, 'D');

                    exit_flag = 1;
                    if(write_to_socket) 
                        write_char(cli_sockfd, EOF_CODE);
                } else if(c == INT_CODE) {
                    write_char(STDIN_FILENO, '^');
                    write_char(STDIN_FILENO, 'C');

                    if(write_to_socket) 
                        write_char(cli_sockfd, INT_CODE);
                } else if(c == LF_CODE || c == CR_CODE) {
                    write_char(STDIN_FILENO, CR_CODE);
                    write_char(STDIN_FILENO, LF_CODE);

                    if(write_to_socket) 
                        write_char(cli_sockfd, LF_CODE);
                } else {
                    write_char(STDIN_FILENO, c);

                    if(write_to_socket) 
                        write_char(cli_sockfd, c);
                }
            }
        }

    }

    if(n_ready < 0) {
        fprintf(stderr, "Error while polling: %s", strerror(errno));
        exit(1);
    }

}

void connect_to_server(int port) {

    struct sockaddr_in serv_addr;
    struct hostent* server;

    // Create socket
    cli_sockfd = socket(AF_INET /*protocol domain*/, SOCK_STREAM /*type*/, 0 /*protocol*/);
    if(cli_sockfd < 0) {
        fprintf(stderr, "Error creating socket: %s", strerror(errno));
        exit(1);
    }

    // Initialize server address struct
    server = gethostbyname("localhost");
    bzero((char *) &serv_addr, sizeof(serv_addr));
    bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(port);

    // Connect to server
    if(connect(cli_sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
        fprintf(stderr, "Error connecting to server: %s", strerror(errno));
        exit(1);
    }

    fprintf(stderr, "CONNECTED TO SERVER\n");

}

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"port", required_argument, 0, 'p'},
        {"log", required_argument, 0, 'l'},
        {"compress", no_argument, 0, 'c'},
        {0, 0, 0, 0}
    };

    char* logfile;
    int c, opt_index, port = -1, compress = 0;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {

        switch(c) {
            case 'p':
                port = atoi(optarg);
                break;
            case 'l':
                logfile = optarg;
                break;
            case 'c':
                compress = 1;
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
        
    }

    if(port < 0) {
        fprintf(stderr, "Need valid port number\n");
        print_usage_and_exit(argv[0]);
    }
    
    connect_to_server(port);

    // Establish socket connection to server here
    struct pollfd p[2] = {
        {STDIN_FILENO, POLL_EVENTS, 0},
        {cli_sockfd, POLL_EVENTS, 0}
    };
    pollfds = p;

    set_terminal_mode();
    process_input();
    exit(0);

}
