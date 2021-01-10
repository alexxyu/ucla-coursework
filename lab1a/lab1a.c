#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <getopt.h>
#include <poll.h>
#include <signal.h>

const int BUFF_SIZE = 256;
const int CR_CODE = 0x0D;
const int LF_CODE = 0x0A;
const int EOF_CODE = 0x04;
const int INT_CODE = 0x03;

const short POLL_EVENTS = POLLIN | POLLHUP | POLLERR;

struct termios newmode;
struct termios currmode;

int pipe_from_shell[2];         // [0] = read end of shell to terminal, [1] = write end
int pipe_from_terminal[2];      // [0] = read end of terminal to shell, [1] = write end

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s [--input INFILE] [--output OUTFILE] [--segfault] [--catch]\n", exec);
    exit(1);
}

void restore_terminal_mode() {
    tcsetattr(0, TCSANOW, &currmode);
}

void set_terminal_mode() {

    int res;

    res = tcgetattr(0, &newmode);

    memcpy(&currmode, &newmode, sizeof(newmode));
    newmode.c_iflag = ISTRIP;	/* only lower 7 bits	*/
    newmode.c_oflag = 0;		/* no processing	*/
    newmode.c_lflag = 0;		/* no processing	*/
    
    tcsetattr(0, TCSANOW, &newmode);

}

void process_input(struct pollfd* pollfds) {

    char buffer[BUFF_SIZE];
    int read_size;

    int exit_flag = 0;

    int n_ready, write_to_shell;
    while( (n_ready = poll(pollfds, 2, 1000)) >= 0 && !exit_flag ) {

        if(n_ready > 0) {
            if(pollfds[0].revents != 0) {
                read_size = read(pollfds[0].fd, buffer, BUFF_SIZE);
                write_to_shell = 1;
            } else if(pollfds[1].revents != 0) {
                read_size = read(pollfds[1].fd, buffer, BUFF_SIZE);
                write_to_shell = 0;
            }

            for(int i=0; i<read_size && !exit_flag; i++) {
                char c = buffer[i];

                if(c == EOF_CODE) {
                    exit_flag = 1;
                } else if(c == INT_CODE) {
                    kill(0, SIGINT);
                } else if(c == LF_CODE || c == CR_CODE) {
                    if(write(1, &(CR_CODE), 1) < 0) {
                        fprintf(stderr, "Write failed: %s\n", strerror(errno));
                        exit(1);
                    }
                    
                    if(write(1, &(LF_CODE), 1) < 0) {
                        fprintf(stderr, "Write failed: %s\n", strerror(errno));
                        exit(1);
                    }

                    if(write_to_shell && write(pipe_from_terminal[1], &(LF_CODE), 1) < 0) {
                        fprintf(stderr, "Write failed: %s\n", strerror(errno));
                        exit(1);
                    }
                } else {
                    if(write(1, &c, 1) < 0) {
                        fprintf(stderr, "Write failed: %s\n", strerror(errno));
                        exit(1);
                    }

                    if(write_to_shell && write(pipe_from_terminal[1], &c, 1) < 0) {
                        fprintf(stderr, "Write failed: %s\n", strerror(errno));
                        exit(1);
                    }
                }
            }
        }

    }

    if(read_size < 0) {
        fprintf(stderr, "Read failed: %s\n", strerror(errno));
        exit(1);
    }

}

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"shell", no_argument, 0, 's'},
        {"debug", no_argument, 0, 'd'},
        {0, 0, 0, 0}
    };

    int c, opt_index;
    int shellflag = 0;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {

        switch(c) {
            case 's':
                shellflag = 1;
                break;
            case 'd':
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
        
    }
    
    if(shellflag) {
        
        pipe(pipe_from_shell);
        pipe(pipe_from_terminal);

        struct pollfd pollfds[2] = {
            {0, POLL_EVENTS, 0},
            {pipe_from_shell[0], POLL_EVENTS, 0}
        };

        int pid;
        switch(pid = fork()) {

            case -1:
                fprintf(stderr, "Error while forking: %s", strerror(errno));
                break;
            case 0:
                // child process
                close(pipe_from_terminal[1]);   
                close(pipe_from_shell[0]);

                close(0);
                dup(pipe_from_terminal[0]);
                close(pipe_from_terminal[0]);

                close(1);
                dup(pipe_from_shell[1]);
                close(2);
                dup(pipe_from_shell[1]);
                close(pipe_from_shell[1]);

                execv("/bin/bash", NULL);
                break;
            default:
                // parent process
                close(pipe_from_terminal[0]);
                close(pipe_from_shell[1]);

                set_terminal_mode();
                process_input(pollfds);
                restore_terminal_mode();
                break;

        }

    }

    if(shellflag) {
        close(pipe_from_terminal[1]);   
        close(pipe_from_shell[0]);

        int status;
        waitpid(-1, &status, 0);
        fprintf(stderr, "SHELL EXIT SIGNAL=%d STATUS=%d\n", status & 0x007f, status & 0xff00);
    }

    exit(0);

}
