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

  char buf[65537] = "Hello, world!";
  int ret = sendto(sock, buf, strlen(buf), 0, (sockaddr*)&addr, socklen);
  if(ret < 0) {
    std::cerr << "ERROR: " << strerror(errno) << std::endl;
    return -1;
  }
  std::cerr << "SENT: " << buf << std::endl;

  close(sock);
  return 0;
}
