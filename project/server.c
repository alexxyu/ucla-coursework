#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>

int PORT = 80;

char* ERR404_HEADER = "HTTP/1.1 404 Not Found \r\n\r\n";
char* ERR404_BODY = "<html><h1>Error 404: Not Found</h1></html>";

void send_response(int socket, char* filename)
{
  FILE* file_fd = fopen(filename, "r");

  if(file_fd == NULL) {
    fprintf(stderr, "Resource %s not found\n", filename);
    send(socket, ERR404_HEADER, strlen(ERR404_HEADER), 0);
    send(socket, ERR404_BODY, strlen(ERR404_BODY), 0);

    // temporary timeout to ensure all bytes received before exiting
    sleep(1);
  } else {
    char header[1024];
  }
}

void process_request(int socket)
{
  // loop and process input
  char* filename = "abc";
  send_response(socket, filename);
}

int connect_to_client(int port)
{
  struct sockaddr_in server_addr, client_addr;

  int server_socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (server_socket < 0) {
    fprintf(stderr, "Error creating socket: %s\n", strerror(errno));
    exit(1);
  }

  bzero(&server_addr, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons(port);
  server_addr.sin_addr.s_addr = INADDR_ANY;

  if( bind(server_socket, (struct sockaddr *) &server_addr, sizeof(server_addr)) < 0 ) {
    fprintf(stderr, "Error binding socket to address: %s\n", strerror(errno));
    exit(1);
  }

  if( listen(server_socket, 1) < 0 ) {
    fprintf(stderr, "Error while listening to socket: %s\n", strerror(errno));
    exit(1);
  }

  socklen_t client_len = sizeof(client_addr);
  int new_socket = accept(server_socket, (struct sockaddr *) &client_addr, &client_len);
  if( new_socket < 0 ) {
    fprintf(stderr, "Error accepting client connection: %s\n", strerror(errno));
    exit(1);
  }

  close(server_socket);

  return new_socket;
}

int main(int argc, char *argv[])
{
  int socket = connect_to_client(PORT);
  process_request(socket);
  close(socket);

  return 0;
}
