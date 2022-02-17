#include <thread>
#include <fstream>
#include <iostream>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>

int main()
{
  sockaddr_in addr;
  socklen_t socklen;

  // TODO: port and filedir
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
  
  int bytes;
  char buf[512];
  std::ofstream ofs("files/1.file");
  while( (bytes = recvfrom(sock, buf, 512, MSG_WAITALL, (sockaddr*)&addr, &socklen)) >= 512 ) {
    ofs.write(buf, bytes);
  }
  ofs.write(buf, bytes);
  ofs.close();
  
  inet_ntop(AF_INET, &addr.sin_addr, ip_str, socklen);
  std::cerr << "Client IP address: " << ip_str << " port: " << ntohs(addr.sin_port) << std::endl;

  close(sock);
  return 0;
}
