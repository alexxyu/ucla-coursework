/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <errno.h>
#include <poll.h>
#include <signal.h>
#include <termios.h>
#include <netdb.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/types.h>
#include "zlib.h"
#include "constants.h"

#define BUFF_SIZE 256

const long FLIMIT = 10000;
const int POLL_TIMEOUT = -1;
const short POLL_EVENTS = POLLIN | POLLHUP | POLLERR;

struct termios newmode;
struct termios currmode;

struct pollfd* pollfds;

char* logfile;
int cli_sockfd;
int logfd, compressflag;

z_stream strm_in, strm_out;
char buffer[BUFF_SIZE], tmp_buffer[BUFF_SIZE];

void print_usage_and_exit(char* exec) {
    fprintf(stderr, "Usage: %s --port=PORTNO [--log=FILENAME] [--compress]\n", exec);
    exit(1);
}

void close_io_on_exit() {
    shutdown(cli_sockfd, SHUT_RDWR);
    if(logfile) {
        close(logfd);
    }

    if(compressflag) {
        deflateEnd(&strm_out);
        inflateEnd(&strm_in);
    }
}

void restore_terminal_mode() {
    if(tcsetattr(STDIN_FILENO, TCSANOW, &currmode) < 0) {
        fprintf(stderr, "Error while setting terminal mode attributes: %s\r\n", strerror(errno));
        exit(1);
    }
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

    if(atexit(restore_terminal_mode) < 0) {
        fprintf(stderr, "Error while registering exit function: %s\r\n", strerror(errno));
        exit(1);
    }

}

void write_char(int fd, const char ch) {

    if(write(fd, &ch, 1) < 0) {
        fprintf(stderr, "Write failed: %s\r\n", strerror(errno));
        exit(1);
    }

}

void write_to_log(char* buffer, int size, int sent) {
    char log_buffer[BUFF_SIZE*2];
    char* action = (sent) ? "SENT" : "RECEIVED";

    // %.*s allows us to specify how many bytes of the buffer to print since it is not null-terminated
    int log_size = sprintf(log_buffer, "%s %d bytes: %.*s\n", action, size, size, buffer);

    if(write(logfd, log_buffer, log_size) < 0) {
        fprintf(stderr, "Error writing to log file: %s\r\n", strerror(errno));
        exit(1);
    }
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

void decompress_message_to_stdout(char* buffer, int read_size, char* decompressed_buffer, int dbuffer_size) {

    int ret;

    strm_in.avail_in = read_size;
    strm_in.next_in = (Bytef*) buffer;

    // Decompress input and write to stdout
    do {
        strm_in.avail_out = dbuffer_size;
        strm_in.next_out = (Bytef*) decompressed_buffer;

        ret = inflate(&strm_in, Z_SYNC_FLUSH);
        if(ret == Z_STREAM_ERROR) {
            fprintf(stderr, "Error decompressing message\r\n");
            exit(1);
        }

        write_buffer_to_stdout(decompressed_buffer, (int) dbuffer_size - strm_in.avail_out);
    } while(strm_in.avail_in > 0);

}

void handle_keyboard_input() {

    bzero(buffer, BUFF_SIZE);

    // Read from keyboard into buffer
    int read_size = read(pollfds[0].fd, buffer, BUFF_SIZE);
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

}

void handle_socket_input() {

    bzero(buffer, BUFF_SIZE);

    // Read from socket into buffer
    int read_size = read(pollfds[1].fd, buffer, BUFF_SIZE);
    if(read_size < 0) {
        fprintf(stderr, "Read failed: %s\r\n", strerror(errno));
        exit(1);
    } else if(read_size == 0) {
        // Extra check for 0 read bytes
        exit(0);
    }

    // Handle logging/decompression if specified
    if(logfile) {
        write_to_log(buffer, read_size, 0);
    }
    
    if(compressflag) {
        memcpy(tmp_buffer, buffer, read_size);
        decompress_message_to_stdout(tmp_buffer, read_size, buffer, BUFF_SIZE);
    } else {
        write_buffer_to_stdout(buffer, read_size);
    }

}

void process_input() {

    int n_ready, exit_flag = 0;
    while( !exit_flag && (n_ready = poll(pollfds, 2, POLL_TIMEOUT)) >= 0 ) {

        if(n_ready > 0) {
            if(pollfds[0].revents & POLLIN) {
                handle_keyboard_input();
            } 
            if(pollfds[1].revents & POLLIN) {
                handle_socket_input();
            }
            if(pollfds[0].revents & POLLHUP || pollfds[0].revents & POLLERR) {
                fprintf(stderr, "Error polling from keyboard: %s\r\n", strerror(errno));
                exit(1);
            }
            if(pollfds[1].revents & POLLHUP || pollfds[1].revents & POLLERR) {
                handle_socket_input();
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
    serv_addr.sin_family = AF_INET;
    bcopy((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(port);

    // Connect to server
    if(connect(cli_sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
        fprintf(stderr, "Error connecting to server: %s\r\n", strerror(errno));
        exit(1);
    }

    // fprintf(stderr, "CONNECTED TO SERVER\r\n");

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

    // Handle extra arguments
    if(optind < argc)
        print_usage_and_exit(argv[0]);

    if(port <= 0) {
        fprintf(stderr, "Please enter a valid port number\n");
        print_usage_and_exit(argv[0]);
    }
    
    connect_to_server(port);

    if(compressflag) {
        initialize_zstreams();
    }
    if(atexit(close_io_on_exit) != 0) {
        fprintf(stderr, "Error while registering exit function: %s\r\n", strerror(errno));
        exit(1);
    } 

    // Establish socket connection to server here
    struct pollfd p[2] = {
        {STDIN_FILENO, POLL_EVENTS, 0},
        {cli_sockfd, POLL_EVENTS, 0}
    };
    pollfds = p;

    // Create log file if specified
    if(logfile) {
        if( (logfd = creat(logfile, 0666)) < 0 ) {
            fprintf(stderr, "Error creating log file: %s\r\n", strerror(errno));
            exit(1);
        }
    }

    set_terminal_mode();
    process_input();
    exit(0);

}
