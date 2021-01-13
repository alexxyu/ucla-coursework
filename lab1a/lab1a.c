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
const char CR_CODE = 0x0D;
const char LF_CODE = 0x0A;
const char EOF_CODE = 0x04;
const char INT_CODE = 0x03;

struct pollfd* pollfds;
const int POLL_TIMEOUT = 0;
const short POLL_EVENTS = POLLIN | POLLHUP | POLLERR;

struct termios newmode;
struct termios currmode;

int pipe_from_shell[2];         // [0] = read end of shell to terminal, [1] = write end
int pipe_from_terminal[2];      // [0] = read end of terminal to shell, [1] = write end

int child_pid;

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

void write_char(int fd, const char ch) {

    // write(2, &ch, 1);

    if(write(fd, &ch, 1) < 0) {
        fprintf(stderr, "Write failed: %s\n", strerror(errno));
        exit(1);
    }

}

void process_input() {

    char buffer[BUFF_SIZE];
    int read_size;

    int exit_flag = 0;

    while( !exit_flag && (read_size = read(0, buffer, BUFF_SIZE)) >= 0 ) {

        for(int i=0; i<read_size && !exit_flag; i++) {
            char c = buffer[i];

            if(c == EOF_CODE) {
                write_char(1, '^');
                write_char(1, 'D');
                exit_flag = 1;
            } else if(c == LF_CODE || c == CR_CODE) {
                write_char(1, CR_CODE);
                write_char(1, LF_CODE);
            } else {
                write_char(1, c);   
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
    if((n_ready = poll(pollfds+1, 1, POLL_TIMEOUT)) >= 0 && pollfds[1].revents & POLLIN) {
        if(n_ready > 0) {
            while((read_size = read(pipe_from_shell[0], buffer, BUFF_SIZE)) > 0) {
                for(int i=0; i<read_size; i++) {
                    char c = buffer[i];

                    write_char(1, c);
                }
            }
        }
    }

    close(pipe_from_shell[0]);
    restore_terminal_mode();

    int status;
    waitpid(child_pid, &status, 0);
    fprintf(stderr, "SHELL EXIT SIGNAL=%d STATUS=%d\n", status & 0x007f, status & 0xff00);

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
            for(int i=0; i<2; i++) {
                if(pollfds[i].revents & POLLIN) {
                    read_size = read(pollfds[i].fd, buffer, BUFF_SIZE);
                    if(read_size < 0) {
                        fprintf(stderr, "Read failed: %s\n", strerror(errno));
                        exit(1);
                    }

                    write_to_shell = 1-i;
                    break;
                } 
            
                if(pollfds[i].revents & POLLHUP || pollfds[i].revents & POLLERR) {
                    fprintf(stderr, "Polling error: %s", strerror(errno));
                    exit(1);
                }
            }

            for(int i=0; !exit_flag && i<read_size; i++) {
                char c = buffer[i];

                if(c == EOF_CODE) {
                    write_char(1, '^');
                    write_char(1, 'D');
                    exit_flag = 1;
                } else if(c == INT_CODE) {
                    write_char(1, '^');
                    write_char(1, 'C');
                    kill(child_pid, SIGINT);
                } else if(c == LF_CODE || c == CR_CODE) {
                    write_char(1, CR_CODE);
                    write_char(1, LF_CODE);

                    if(write_to_shell) 
                        write_char(pipe_from_terminal[1], LF_CODE);
                } else {
                    write_char(1, c);
                    
                    if(write_to_shell) 
                        write_char(pipe_from_terminal[1], c);                
                }
            }
        }

    }

    cleanup();

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
    
    set_terminal_mode();
    if(shellflag) {
        
        pipe(pipe_from_shell);
        pipe(pipe_from_terminal);

        struct pollfd p[2] = {
            {0, POLL_EVENTS, 0},
            {pipe_from_shell[0], POLL_EVENTS, 0}
        };
        pollfds = p;

        switch(child_pid = fork()) {

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

                process_input_with_shell();
                break;

        }

    } else {
        process_input();
    }

    restore_terminal_mode();

    exit(0);

}
