#include <cstring>
#include <thread>
#include <iostream>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>

int main()
{
  sockaddr_in addr;
  socklen_t socklen;

  // TODO: port, ip_str, filename
  int port = 5000;
  char ip_str[65537] = "localhost";

  int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if(sock < 0) {
    std::cerr << "ERROR: " << strerror(errno) << std::endl;
    return -1;
  }

  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = inet_addr("127.0.0.1");
  socklen = sizeof(addr);

  FILE* file = fopen("client.cpp", "r");

  char buf[512];
  int bytes_read;
  while( (bytes_read = fread(buf, 1, 512, file)) > 0 ) {
    if( sendto(sock, buf, bytes_read, 0, (sockaddr*)&addr, socklen) < 0) {
      std::cerr << "ERROR: " << strerror(errno) << std::endl;
      return -1;
    }
  }

  close(sock);
  return 0;
}
