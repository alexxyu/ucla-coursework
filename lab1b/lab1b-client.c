/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

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
#include <fcntl.h>
#include <ulimit.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/types.h>
#include "zlib.h"
#include "constants.h"

struct pollfd* pollfds;
const int POLL_TIMEOUT = -1;
const short POLL_EVENTS = POLLIN | POLLHUP | POLLERR;

const long FLIMIT = 10000;

struct termios newmode;
struct termios currmode;

int cli_sockfd;

int logfd, compressflag;
char* logfile;

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s --port=INT [--log=FILENAME] [--compress]\n", exec);
    exit(1);
}

void restore_terminal_mode() {
    if(tcsetattr(STDIN_FILENO, TCSANOW, &currmode) < 0) {
        fprintf(stderr, "Error while setting terminal mode attributes: %s\r\n", strerror(errno));
        exit(1);
    }

    if(logfile) close(logfd);
}

void set_terminal_mode() {

    if(tcgetattr(STDIN_FILENO, &newmode) < 0) {
        fprintf(stderr, "Error while getting terminal mode attributes: %s\r\n", strerror(errno));
        exit(1);
    }

    memcpy(&currmode, &newmode, sizeof(newmode));
    newmode.c_iflag = ISTRIP;	/* only lower 7 bits	*/
    newmode.c_oflag = 0;		/* no processing	*/
    newmode.c_lflag = 0;		/* no processing	*/
    
    if(tcsetattr(STDIN_FILENO, TCSANOW, &newmode) < 0) {
        fprintf(stderr, "Error while setting terminal mode attributes: %s\r\n", strerror(errno));
        exit(1);
    }
    atexit(restore_terminal_mode);

}

void write_char(int fd, const char ch) {

    if(write(fd, &ch, 1) < 0) {
        fprintf(stderr, "Write failed: %s\r\n", strerror(errno));
        exit(1);
    }

}

void write_to_log(char* buffer, int size, int sent) {
    char log_buffer[BUFF_SIZE];
    buffer[size] = '\0';

    char* action = (sent) ? "SENT" : "RECEIVED";
    int log_size = sprintf(log_buffer, "%s %d bytes: %s\n", action, size, buffer);

    if(write(logfd, log_buffer, log_size) < 0) {
        fprintf(stderr, "Error writing to log file: %s\r\n", strerror(errno));
        exit(1);
    }
}

int compress_message(char* buffer, int read_size, char* compressed_buffer, int cbuffer_size) {

    int ret;
    z_stream strm;

    // Initialize zlib stream parameters
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;

    ret = deflateInit(&strm, Z_DEFAULT_COMPRESSION);
    if(ret != Z_OK) {
        fprintf(stderr, "Error initializing zlib\n");
        exit(1);
    }

    strm.avail_in = read_size;
    strm.next_in = (Bytef*) buffer;
    strm.avail_out = cbuffer_size;
    strm.next_out = (Bytef*) compressed_buffer;

    // Compress input into compressed buffer
    do {
        ret = deflate(&strm, Z_SYNC_FLUSH);
        if(ret == Z_STREAM_ERROR) {
            fprintf(stderr, "Error compressing message\n");
            deflateEnd(&strm);
        }
    } while(strm.avail_in > 0);

    // Clean up and return read size of compressed buffer
    deflateEnd(&strm);
    return cbuffer_size - strm.avail_out;

}

int decompress_message(char* buffer, int read_size, char* decompressed_buffer, int dbuffer_size) {

    int ret;

    // Initialize zlib stream parameters
    z_stream strm;
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;

    ret = inflateInit(&strm);
    if(ret != Z_OK) {
        fprintf(stderr, "Error initializing zlib\n");
        exit(1);
    }

    strm.avail_in = read_size;
    strm.next_in = (Bytef*) buffer;
    strm.avail_out = dbuffer_size;
    strm.next_out = (Bytef*) decompressed_buffer;

    // Decompress input into decompressed buffer
    do {
        ret = inflate(&strm, Z_SYNC_FLUSH);
        if(ret == Z_STREAM_ERROR) {
            fprintf(stderr, "Error decompressing message\n");
            inflateEnd(&strm);
        }
    } while(strm.avail_in > 0);

    // Clean up and return read size of decompressed buffer
    inflateEnd(&strm);
    return dbuffer_size - strm.avail_out;

}

void write_buffer_to_stdout(char* buffer, int read_size) {

    for(int i=0; i<read_size; i++) {
        char c = buffer[i];

        if(c == EOF_CODE) {
            write_char(STDOUT_FILENO, '^');
            write_char(STDOUT_FILENO, 'D');
        } else if(c == INT_CODE) {
            write_char(STDOUT_FILENO, '^');
            write_char(STDOUT_FILENO, 'C');
        } else if(c == LF_CODE || c == CR_CODE) {
            write_char(STDOUT_FILENO, CR_CODE);
            write_char(STDOUT_FILENO, LF_CODE);
        } else {
            write_char(STDOUT_FILENO, c);
        }
    }

}

void process_input() {

    char buffer[BUFF_SIZE], tmp_buffer[BUFF_SIZE];

    int read_size, n_ready, exit_flag = 0;
    while( !exit_flag && (n_ready = poll(pollfds, 2, POLL_TIMEOUT)) >= 0 ) {

        if(n_ready > 0) {
            if(pollfds[0].revents & POLLIN) {

                // Handle keyboard input
                read_size = read(pollfds[0].fd, buffer, BUFF_SIZE);
                if(read_size < 0) {
                    fprintf(stderr, "Read failed: %s\r\n", strerror(errno));
                    exit(1);
                }

                write_buffer_to_stdout(buffer, read_size);

                // Handle logging/compression if specified and write to server
                if(compressflag) {
                    memcpy(tmp_buffer, buffer, read_size);
                    read_size = compress_message(tmp_buffer, read_size, buffer, BUFF_SIZE);
                } 

                if(logfile) {
                    write_to_log(buffer, read_size, 1);
                }
                
                write(cli_sockfd, buffer, read_size);

            } else if(pollfds[1].revents & POLLIN) {

                // Handle socket input
                read_size = read(pollfds[1].fd, buffer, BUFF_SIZE);
                if(read_size < 0) {
                    fprintf(stderr, "Read failed: %s\r\n", strerror(errno));
                    exit(1);
                }

                // Handle logging/decompression if specified
                if(logfile) {
                    write_to_log(buffer, read_size, 0);
                }

                if(compressflag) {
                    memcpy(tmp_buffer, buffer, read_size);
                    read_size = decompress_message(tmp_buffer, read_size, buffer, BUFF_SIZE);
                }

                write_buffer_to_stdout(buffer, read_size);

            }

            if(pollfds[0].revents & POLLHUP || pollfds[0].revents & POLLERR) {
                fprintf(stderr, "Error polling from keyboard: %s\r\n", strerror(errno));
                exit(1);
            }

            if(pollfds[1].revents & POLLHUP || pollfds[1].revents & POLLERR) {
                exit_flag = 1;
            }
        }

    }

    if(n_ready < 0) {
        fprintf(stderr, "Error while polling: %s\r\n", strerror(errno));
        exit(1);
    }

}

void connect_to_server(int port) {

    struct sockaddr_in serv_addr;
    struct hostent* server;

    // Create socket
    cli_sockfd = socket(AF_INET /*protocol domain*/, SOCK_STREAM /*type*/, 0 /*protocol*/);
    if(cli_sockfd < 0) {
        fprintf(stderr, "Error creating socket: %s\r\n", strerror(errno));
        exit(1);
    }

    // Initialize server address struct
    server = gethostbyname("localhost");
    bzero((char *) &serv_addr, sizeof(serv_addr));
    bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(port);

    // Connect to server
    if(connect(cli_sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
        fprintf(stderr, "Error connecting to server: %s\r\n", strerror(errno));
        exit(1);
    }

    fprintf(stderr, "CONNECTED TO SERVER\n\r");

}

int main(int argc, char *argv[]) {

    logfile = NULL;
    compressflag = 0;

    const struct option long_options[] = {
        {"port", required_argument, 0, 'p'},
        {"log", required_argument, 0, 'l'},
        {"compress", no_argument, 0, 'c'},
        {0, 0, 0, 0}
    };

    int c, opt_index, port = -1;
    while( (c = getopt_long(argc, argv, "", long_options, &opt_index)) != -1 ) {

        switch(c) {
            case 'p':
                port = (int) strtol(optarg, NULL, 10);
                break;
            case 'l':
                logfile = optarg;
                break;
            case 'c':
                compressflag = 1;
                break;
            default:
                // Handle unrecognized argument
                print_usage_and_exit(argv[0]);
        }
        
    }

    if(port <= 0) {
        fprintf(stderr, "Please enter a valid port number.\n");
        print_usage_and_exit(argv[0]);
    }
    
    connect_to_server(port);

    // Establish socket connection to server here
    struct pollfd p[2] = {
        {STDIN_FILENO, POLL_EVENTS, 0},
        {cli_sockfd, POLL_EVENTS, 0}
    };
    pollfds = p;

    // Create log file
    if(logfile) {
        ulimit(UL_SETFSIZE, FLIMIT);
        logfd = creat(logfile, 0666);
        if(logfd < 0) {
            fprintf(stderr, "Error creating log file: %s\r\n", strerror(errno));
            exit(1);
        }
    }

    set_terminal_mode();
    process_input();
    exit(0);

}
