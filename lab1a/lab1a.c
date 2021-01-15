#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <getopt.h>
#include <poll.h>
#include <signal.h>
#include <sys/wait.h>

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

    int read_size;
    char buffer[BUFF_SIZE];

    close(pipe_from_terminal[1]);   

    // Process any remaining input from shell
    while(poll(pollfds+1, 1, POLL_TIMEOUT) >= 0 && !(pollfds[1].revents & POLLHUP) 
          && !(pollfds[1].revents & POLLERR)) {
        if(pollfds[1].revents & POLLIN) {
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

int main(int argc, char *argv[]) {

    const struct option long_options[] = {
        {"shell", no_argument, 0, 's'},
        {0, 0, 0, 0}
    };

    int c, opt_index;
    int shellflag = 0;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {

        switch(c) {
            case 's':
                shellflag = 1;
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

    } else {
        process_input();
    }

    exit(0);

}
