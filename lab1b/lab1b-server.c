/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

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
#include "zlib.h"
#include "constants.h"

#define BUFF_SIZE 256

struct pollfd* pollfds;
const int POLL_TIMEOUT = -1;
const short POLL_EVENTS = POLLIN | POLLHUP | POLLERR;

int child_pid;
int pipe_from_shell[2];         // [0] = read end of shell to terminal, [1] = write end
int pipe_from_terminal[2];      // [0] = read end of terminal to shell, [1] = write end
int write_pipe_is_open;

int exit_flag;
int compressflag;
int serv_sockfd, serv_sockfd_new;

z_stream strm_in, strm_out;
char buffer[BUFF_SIZE], tmp_buffer[BUFF_SIZE];

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s --port=PORTNO [--compress]\n", exec);
    exit(1);
}

void close_io_on_exit() {
    shutdown(serv_sockfd, SHUT_RDWR);
    shutdown(serv_sockfd_new, SHUT_RDWR);

    close(pipe_from_shell[0]);

    if(write_pipe_is_open) {
        close(pipe_from_terminal[1]);
    }

    if(compressflag) {
        deflateEnd(&strm_out);
        inflateEnd(&strm_in);
    }
}

void initialize_zstreams() {

    int ret;

    // Initialize zlib stream parameters
    strm_out.zalloc = Z_NULL;
    strm_out.zfree = Z_NULL;
    strm_out.opaque = Z_NULL;
    ret = deflateInit(&strm_out, Z_DEFAULT_COMPRESSION);
    if(ret != Z_OK) {
        fprintf(stderr, "Error initializing zlib stream\r\n");
        exit(1);
    }

    strm_in.zalloc = Z_NULL;
    strm_in.zfree = Z_NULL;
    strm_in.opaque = Z_NULL;
    strm_in.avail_in = 0;
    strm_in.next_in = Z_NULL;
    ret = inflateInit(&strm_in);
    if(ret != Z_OK) {
        fprintf(stderr, "Error initializing zlib stream\r\n");
        exit(1);
    }

}

int compress_message(char* buffer, int read_size, char* compressed_buffer, int cbuffer_size) {

    int ret;

    strm_out.avail_in = read_size;
    strm_out.next_in = (Bytef*) buffer;
    strm_out.avail_out = cbuffer_size;
    strm_out.next_out = (Bytef*) compressed_buffer;

    // Compress input into compressed buffer
    ret = deflate(&strm_out, Z_SYNC_FLUSH);
    if(ret == Z_STREAM_ERROR) {
        fprintf(stderr, "Error compressing message\r\n");
        exit(1);
    }

    // Return read size of compressed buffer
    return cbuffer_size - strm_out.avail_out;

}

int decompress_message(char* buffer, int read_size, char* decompressed_buffer, int dbuffer_size) {

    int ret;

    strm_in.avail_in = read_size;
    strm_in.next_in = (Bytef*) buffer;
    strm_in.avail_out = dbuffer_size;
    strm_in.next_out = (Bytef*) decompressed_buffer;

    // Decompress input into decompressed buffer
    ret = inflate(&strm_in, Z_SYNC_FLUSH);
    if(ret == Z_STREAM_ERROR) {
        fprintf(stderr, "Error decompressing message\r\n");
        exit(1);
    }

    // Clean up and return read size of decompressed buffer
    return dbuffer_size - strm_in.avail_out;

}

void write_char(int fd, const char ch) {

    if(write(fd, &ch, 1) < 0) {
        fprintf(stderr, "Write failed: %s\n", strerror(errno));
        exit(1);
    }

}

void handle_client_input() {

    bzero(buffer, BUFF_SIZE);

    // Read from client into buffer
    int read_size = read(pollfds[0].fd, buffer, BUFF_SIZE);
    if(read_size < 0) {
        fprintf(stderr, "Read failed: %s\n", strerror(errno));
        exit(1);
    }

    // Decompress if specified
    if(compressflag) {
        memcpy(tmp_buffer, buffer, read_size);
        read_size = decompress_message(tmp_buffer, read_size, buffer, BUFF_SIZE);
    }

    // Handle special input and send to shell
    for(int i=0; !exit_flag && i<read_size; i++) {
        char c = buffer[i];

        if(c == EOF_CODE) {
            exit_flag = 1;
        } else if(c == INT_CODE) {
            if(kill(child_pid, SIGINT) < 0) {
                fprintf(stderr, "Error while interrupting child process: %s\n", strerror(errno));
                exit(1);
            }
        } else if(c == LF_CODE || c == CR_CODE) {
            write_char(pipe_from_terminal[1], LF_CODE);
        } else {
            write_char(pipe_from_terminal[1], c); 
        }
    }

}

void handle_shell_input() {

    bzero(buffer, BUFF_SIZE);

    // Read from shell into buffer
    int read_size = read(pollfds[1].fd, buffer, BUFF_SIZE);
    if(read_size < 0) {
        fprintf(stderr, "Read failed: %s\n", strerror(errno));
        exit(1);
    }

    // Compress if specified and send shell input back to client
    if(compressflag) {
        memcpy(tmp_buffer, buffer, read_size);
        read_size = compress_message(tmp_buffer, read_size, buffer, BUFF_SIZE);

        write(serv_sockfd_new, buffer, read_size);
    } else {
        write(serv_sockfd_new, buffer, read_size);
    }

}

void cleanup() {

    close(pipe_from_terminal[1]);
    write_pipe_is_open = 0;

    exit_flag = 0;
    while(!exit_flag && poll(pollfds+1, 1, POLL_TIMEOUT) >= 0 && !(pollfds[1].revents & POLLHUP) 
          && !(pollfds[1].revents & POLLERR)) {
        handle_shell_input();
    }

    // Harvest shell's completion status
    int status;
    if(waitpid(child_pid, &status, 0) < 0) {
        fprintf(stderr, "Error while waiting for child process: %s\n", strerror(errno));
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

    exit_flag = 0;
    if(signal(SIGPIPE, handle_sigpipe) == SIG_ERR) {
        fprintf(stderr, "Error while setting SIGPIPE handler: %s\n", strerror(errno));
        exit(1);
    }

    int n_ready;
    while( !exit_flag && (n_ready = poll(pollfds, 2, POLL_TIMEOUT)) >= 0 ) {

        if(n_ready > 0) {
            if(pollfds[0].revents & POLLIN) {
                handle_client_input();
            }
            if(pollfds[1].revents & POLLIN) {
                handle_shell_input();
            }
            if(pollfds[0].revents & POLLHUP || pollfds[0].revents & POLLERR || 
               pollfds[1].revents & POLLHUP || pollfds[1].revents & POLLERR) {
                exit_flag = 1;
            }
        }

    }
    cleanup();

}

void connect_to_client(int port) {

    struct sockaddr_in serv_addr, cli_addr;

    // Create socket
    serv_sockfd = socket(AF_INET /*protocol domain*/, SOCK_STREAM /*type*/, 0 /*protocol*/);
    if(serv_sockfd < 0) {
        fprintf(stderr, "Error creating socket: %s\n", strerror(errno));
        exit(1);
    }

    // Initialize server addr struct
    bzero(&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    serv_addr.sin_addr.s_addr = INADDR_ANY;

    // Bind socket to server address
    if(bind(serv_sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
        fprintf(stderr, "Error binding socket to address: %s\n", strerror(errno));
        exit(1);
    }

    // Listen for connection and accept
    if(listen(serv_sockfd, 5) < 0) {
        fprintf(stderr, "Error while listening to socket: %s\n", strerror(errno));
        exit(1);
    }

    // Accept client connection
    socklen_t cli_len = sizeof(cli_addr);
    serv_sockfd_new = accept(serv_sockfd, (struct sockaddr *) &cli_addr, &cli_len);
    if(serv_sockfd_new < 0) {
        fprintf(stderr, "Error accepting client connection: %s\n", strerror(errno));
        exit(1);
    }

    // fprintf(stderr, "ACCEPTED CLIENT CONNECTION\n");

}

int main(int argc, char *argv[]) {

    compressflag = 0;

    const struct option long_options[] = {
        {"port", required_argument, 0, 'p'},
        {"compress", no_argument, 0, 'c'},
        {0, 0, 0, 0}
    };

    int c, opt_index, port = -1;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {

        switch(c) {
            case 'p':
                port = (int) strtol(optarg, NULL, 10);
                break;
            case 'c':
                compressflag = 1;
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
        
    }

    // Handle extra arguments
    if(optind < argc) {
        print_usage_and_exit(argv[0]);
    }
    
    if(port <= 0) {
        fprintf(stderr, "Please enter a valid port number.\n");
        print_usage_and_exit(argv[0]);
    }

    connect_to_client(port);

    if(pipe(pipe_from_shell) < 0) {
        fprintf(stderr, "Error during pipe creation: %s\n", strerror(errno));
        exit(1);
    }
    if(pipe(pipe_from_terminal) < 0) {
        fprintf(stderr, "Error during pipe creation: %s\n", strerror(errno));
        exit(1);
    }
    write_pipe_is_open = 1;

    struct pollfd p[2] = {
        {serv_sockfd_new, POLL_EVENTS, 0},
        {pipe_from_shell[0], POLL_EVENTS, 0}
    };
    pollfds = p;

    if(compressflag) {
        initialize_zstreams();
    }

    switch(child_pid = fork()) {

        case -1:
            fprintf(stderr, "Error while forking: %s\r\n", strerror(errno));
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

            if(atexit(close_io_on_exit) < 0) {
                fprintf(stderr, "Error while registering exit function: %s\r\n", strerror(errno));
                exit(1);
            }

            process_input_with_shell();
            break;

    }

    exit(0);

}
