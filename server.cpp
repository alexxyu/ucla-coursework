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

  // TODO: port and ip_str
  int port = 5000;
  char ip_str[65537] = "localhost"; 

  int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if(sock < 0) {
    std::cerr << "ERROR: " << strerror(errno) << std::endl;
    return -1;
  }

  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  socklen = sizeof(addr);

  if( bind(sock, (sockaddr*)&addr, socklen) < 0 ) {
    std::cerr << "ERROR: " << strerror(errno) << std::endl;
    return -1;
  }
  
  char buf[65537];
  int ret = recvfrom(sock, buf, 65537, 0, (sockaddr*)&addr, &socklen);
  buf[ret] = 0;
  std::cerr << "RECEIVED: " << buf << std::endl;

  inet_ntop(AF_INET, &addr.sin_addr, ip_str, socklen);
  std::cerr << "Client IP address: " << ip_str << " port: " << ntohs(addr.sin_port) << std::endl;

  close(sock);
  return 0;
}
