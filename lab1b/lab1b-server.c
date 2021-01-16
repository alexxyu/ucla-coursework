#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <getopt.h>
#include <poll.h>
#include <signal.h>
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

int pipe_from_shell[2];         // [0] = read end of shell to terminal, [1] = write end
int pipe_from_terminal[2];      // [0] = read end of terminal to shell, [1] = write end

int serv_sockfd, cli_sockfd;
int child_pid;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s [--input INFILE] [--output OUTFILE] [--segfault] [--catch]\n", exec);
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

    while( !exit_flag && (read_size = read(STDIN_FILENO, buffer, BUFF_SIZE)) >= 0 ) {

        for(int i=0; i<read_size && !exit_flag; i++) {
            char c = buffer[i];

            if(c == EOF_CODE) {
                write_char(STDOUT_FILENO, '^');
                write_char(STDOUT_FILENO, 'D');
                exit_flag = 1;
            } else if(c == LF_CODE || c == CR_CODE) {
                write_char(STDOUT_FILENO, CR_CODE);
                write_char(STDOUT_FILENO, LF_CODE);
            } else {
                write_char(STDOUT_FILENO, c);   
            }
        }

    }

    if(read_size < 0) {
        fprintf(stderr, "Read failed: %s\n", strerror(errno));
        exit(1);
    }

}

void cleanup() {

    int read_size, n_ready;
    char buffer[BUFF_SIZE];

    close(pipe_from_terminal[1]);   

    // Process any remaining input from shell
    while((n_ready = poll(pollfds+1, 1, POLL_TIMEOUT)) >= 0 && !(pollfds[1].revents & POLLHUP) 
          && !(pollfds[1].revents & POLLERR)) {
        if(n_ready > 0 && (pollfds[1].revents & POLLIN)) {
            while((read_size = read(pipe_from_shell[0], buffer, BUFF_SIZE)) > 0) {
                for(int i=0; i<read_size; i++) {
                    char c = buffer[i];

                    write_char(STDOUT_FILENO, c);
                }
            }
        }
    }

    close(pipe_from_shell[0]);

    int status;
    if(waitpid(child_pid, &status, 0) < 0) {
        fprintf(stderr, "Error while waiting for child process: %s", strerror(errno));
        exit(1);
    }
    fprintf(stderr, "SHELL EXIT SIGNAL=%d STATUS=%d\n", WTERMSIG(status), WEXITSTATUS(status));

    exit(0);

}

void handle_sigpipe() {
    // fprintf(stderr, "\nSTATUS: %d\t%d\t%d\n", pollfds[1].revents & POLLIN, 
    //                                           pollfds[1].revents & POLLHUP, 
    //                                           pollfds[1].revents & POLLERR);
    cleanup();
}

void process_input_with_shell() {

    char buffer[BUFF_SIZE];
    int read_size;

    signal(SIGPIPE, handle_sigpipe);

    int exit_flag = 0;
    int n_ready, write_to_shell;
    while( !exit_flag && (n_ready = poll(pollfds, 2, POLL_TIMEOUT)) >= 0 ) {

        if(n_ready > 0) {
            if(pollfds[0].revents & POLLIN) {
                // Handle keyboard input
                read_size = read(pollfds[0].fd, buffer, BUFF_SIZE);
                if(read_size < 0) {
                    fprintf(stderr, "Read failed: %s\n", strerror(errno));
                    exit(1);
                }

                write_to_shell = 1;
            }

            if(pollfds[0].revents & POLLHUP || pollfds[0].revents & POLLERR) {
                fprintf(stderr, "Error polling from keyboard: %s", strerror(errno));
                exit(1);
            }

            if(pollfds[1].revents & POLLIN) {
                // Handle shell input
                read_size = read(pollfds[1].fd, buffer, BUFF_SIZE);
                if(read_size < 0) {
                    fprintf(stderr, "Read failed: %s\n", strerror(errno));
                    exit(1);
                }

                write_to_shell = 0;
            }

            if(pollfds[1].revents & POLLHUP || pollfds[1].revents & POLLERR) {
                // Receipt of polling error means no more output from shell
                exit_flag = 1;
            }

            // Write character to terminal stdout + shell if applicable
            for(int i=0; !exit_flag && i<read_size; i++) {
                char c = buffer[i];

                if(c == EOF_CODE) {
                    write_char(STDOUT_FILENO, '^');
                    write_char(STDOUT_FILENO, 'D');
                    exit_flag = 1;
                } else if(c == INT_CODE) {
                    write_char(STDOUT_FILENO, '^');
                    write_char(STDOUT_FILENO, 'C');
                    if(kill(child_pid, SIGINT) < 0) {
                        fprintf(stderr, "Error while interrupting child process: %s", strerror(errno));
                        exit(1);
                    }
                } else if(c == LF_CODE || c == CR_CODE) {
                    write_char(STDOUT_FILENO, CR_CODE);
                    write_char(STDOUT_FILENO, LF_CODE);

                    if(write_to_shell) 
                        write_char(pipe_from_terminal[1], LF_CODE);
                } else {
                    write_char(STDOUT_FILENO, c);
                    
                    if(write_to_shell) 
                        write_char(pipe_from_terminal[1], c);                
                }
            }
        }

    }

    cleanup();

}

void connect_to_client(int port) {

    struct sockaddr_in serv_addr, cli_addr;

    serv_sockfd = socket(AF_INET /*protocol domain*/, SOCK_STREAM /*type*/, 0 /*protocol*/);
    if(serv_sockfd < 0) {
        fprintf(stderr, "Error creating socket: %s", strerror(errno));
        exit(1);
    }

    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = INADDR_ANY;

    if(bind(serv_sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
        fprintf(stderr, "Error binding socket to address: %s", strerror(errno));
        exit(1);
    }

    listen(serv_sockfd, 5);

    socklen_t cli_len = sizeof(cli_addr);
    cli_sockfd = accept(serv_sockfd, (struct sockaddr *) &cli_addr, &cli_len);
    if(cli_sockfd < 0) {
        fprintf(stderr, "Error accepting client connection: %s", strerror(errno));
        exit(1);
    }

    fprintf(stderr, "ACCEPTED CLIENT CONNECTION\n");

}

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"port", required_argument, 0, 'p'},
        {"compress", no_argument, 0, 'c'},
        {0, 0, 0, 0}
    };

    int c, opt_index, port, compress = 0;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {

        switch(c) {
            case 'p':
                port = atoi(optarg);
                break;
            case 'c':
                compress = 1;
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
        
    }
    
    connect_to_client(port);

    set_terminal_mode();
    pipe(pipe_from_shell);
    pipe(pipe_from_terminal);

    struct pollfd p[2] = {
        {STDIN_FILENO, POLL_EVENTS, 0},
        {pipe_from_shell[0], POLL_EVENTS, 0}
    };
    pollfds = p;

    switch(child_pid = fork()) {

        case -1:
            fprintf(stderr, "Error while forking: %s", strerror(errno));
            break;
        case 0:
            // child process: redirect stdio to pipes and replace with bash
            close(pipe_from_terminal[1]);   
            close(pipe_from_shell[0]);

            close(STDIN_FILENO);
            dup(pipe_from_terminal[0]);
            close(pipe_from_terminal[0]);

            close(STDOUT_FILENO);
            dup(pipe_from_shell[1]);
            close(STDERR_FILENO);
            dup(pipe_from_shell[1]);
            close(pipe_from_shell[1]);

            char *args[] = { "/bin/bash", NULL };
            execv("/bin/bash", args);
            break;
        default:
            // parent process: begin processing keyboard input
            close(pipe_from_terminal[0]);
            close(pipe_from_shell[1]);

            process_input_with_shell();
            break;

    }

    exit(0);

}
