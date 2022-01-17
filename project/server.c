#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>

const int PORT = 8080;
const int BUF_SIZE = 1024;
const char* ERR404_HEADER = "HTTP/1.1 404 Not Found\r\n\r\n";
const char* ERR404_BODY = "<html><h1>Error 404: Not Found</h1></html>";

void send_response(int socket, char* filename)
{
  FILE* file_fd = fopen(filename, "r");

  if(file_fd == NULL) {
    // Resource does not exist
    fprintf(stderr, "SERVER: Resource '%s' not found\n", filename);
    send(socket, ERR404_HEADER, strlen(ERR404_HEADER), 0);
    send(socket, ERR404_BODY, strlen(ERR404_BODY), 0);
  } else {
    // Send header
    char header[BUF_SIZE];
    sprintf(header, "HTTP/1.1 200 OK\r\n\r\n");
    send(socket, header, strlen(header), 0);

    // Read from file and send as body 
    char body[BUF_SIZE];

    int total_bytes = 0, bytes_read;
    while( (bytes_read = fread(body, 1, BUF_SIZE, file_fd)) > 0 ) {
      send(socket, body, bytes_read, 0);
      total_bytes += bytes_read;
    }
    
    fprintf(stderr, "SERVER: %d bytes sent for resource '%s'\n", total_bytes, filename);
    fclose(file_fd);
  }
}

void process_request(int socket)
{
  char msg[BUF_SIZE];
  int bytes_read = recv(socket, msg, BUF_SIZE, 0);
  if(bytes_read < 0) {
    fprintf(stderr, "SERVER: Error while receiving message from socket: %s\n", strerror(errno));
    return;
  }
  msg[bytes_read] = '\0';
  fprintf(stderr, "SERVER: Received following message from client:\n%s", msg);

  char filename[BUF_SIZE];
  if(sscanf(msg, "GET %s HTTP/1.1\r\n", filename) != 1) {
    // HTTP request does not match correct format
    fprintf(stderr, "SERVER: Invalid GET request\n");
  } else {
    // Replace URL encoded spaces ('%20') with actual spaces
    char filename_new[BUF_SIZE];
    const char* delim = "%20";
    char* token;

    token = strtok(filename, delim);
    strcat(filename_new, token);
    while( (token = strtok(NULL, delim)) != NULL ) {
      strcat(filename_new, " ");
      strcat(filename_new, token);
    } 

    // Send response back to client (also, ignore first '/' in filename)
    send_response(socket, filename_new+1);
  }
}

int main(int argc, char *argv[])
{
  struct sockaddr_in server_addr, client_addr;
  socklen_t client_len = sizeof(client_addr);

  bzero(&server_addr, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons(PORT);
  server_addr.sin_addr.s_addr = INADDR_ANY;

  int sockfd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (sockfd < 0) {
    fprintf(stderr, "SERVER: Error creating socket: %s\n", strerror(errno));
    exit(1);
  }

  if( bind(sockfd, (struct sockaddr *) &server_addr, sizeof(server_addr)) < 0 ) {
    fprintf(stderr, "SERVER: Error binding socket to address: %s\n", strerror(errno));
    exit(1);
  }

  if( listen(sockfd, 1) < 0 ) {
    fprintf(stderr, "SERVER: Error while listening to socket: %s\n", strerror(errno));
    exit(1);
  }

  while(1) {
    int new_sockfd = accept(sockfd, (struct sockaddr *) &client_addr, &client_len);
    if( new_sockfd < 0 ) {
      fprintf(stderr, "SERVER: Error accepting client connection: %s\n", strerror(errno));
      exit(1);
    }

    int child_pid;
    switch(child_pid = fork()) {
      case -1:
        fprintf(stderr, "SERVER: Error while forking process: %s\n", strerror(errno));
        exit(1);
      case 0:
        // Child process: handle incoming HTTP request
        fprintf(stderr, "SERVER: Accepted connection from client\n");
        process_request(new_sockfd);
        close(new_sockfd);
        exit(0);
      default:
        // Parent process: accept next connection
        close(new_sockfd);
        break;
    }
  }

  close(sockfd);
  return 0;
}
