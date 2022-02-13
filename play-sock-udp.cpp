#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <arpa/inet.h>
#include <sys/socket.h>

char cmd[65537];
char ip_str[65537];
char buf[65537];

int main(){
  int sock = -1;
  sockaddr_in addr;
  socklen_t socklen;
  int port, buflen;

  for(printf("> "); fgets(cmd, sizeof(cmd), stdin) != NULL; printf("> ")){
    if(strncmp(cmd, "exit", 4) == 0){
      break;
    } else if(strncmp(cmd, "socket", 6) == 0){
      // Print code
      printf("sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);\n");

      // Create socket: IPv4 family, Stream socket, TCP protocol
      // Document: https://man7.org/linux/man-pages/man2/socket.2.html
      sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
      printf("sock == %d\n", sock);
      // -1 means error
      if(sock == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      }

      // Set reuse address and reuse port in case to demo. Don't do this in your project 2
      int enable = 1;
      setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(int));
      // setsockopt(sock, SOL_SOCKET, SO_REUSEPORT, &enable, sizeof(int));
    } else if(strncmp(cmd, "connect", 7) == 0) {
      if(sscanf(cmd, "connect %d %s %d", &sock, ip_str, &port) != 3){
        printf("Usage: connect SOCKET IP PORT\n");
        continue;
      }

      // Print code
      printf("sockaddr_in addr;\n");
      printf("addr.sin_family = AF_INET;\n");
      printf("addr.sin_port = htons(%d);\n", port);
      printf("inet_pton(AF_INET, %s, &addr.sin_addr);\n", ip_str);

      // IPv4 family
      addr.sin_family = AF_INET;
      // Port number. Note that IP uses big-endian.
      addr.sin_port = htons(port);
      // Convert IP address from string to binary form
      if(!inet_pton(AF_INET, ip_str, &addr.sin_addr)){
        printf("Wrong IP address\n");
        continue;
      }

      // Print code
      printf("ret = connect(%d, (sockaddr*)&addr, sizeof(addr));\n", sock);

      // Connect to a server
      // Document: https://man7.org/linux/man-pages/man2/connect.2.html
      int ret = connect(sock, (sockaddr*)&addr, sizeof(addr));
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      }
    } else if(strncmp(cmd, "bind", 4) == 0) {
      if(sscanf(cmd, "bind %d %s %d", &sock, ip_str, &port) != 3){
        printf("Usage: bind SOCKET IP PORT\n");
        continue;
      }

      // Print code
      printf("sockaddr_in addr;\n");
      printf("addr.sin_family = AF_INET;\n");
      printf("addr.sin_port = htons(%d);\n", port);

      addr.sin_family = AF_INET;
      addr.sin_port = htons(port);

      if(strcmp(ip_str, "any") == 0){
        printf("addr.sin_addr.s_addr = INADDR_ANY;\n");
        addr.sin_addr.s_addr = htonl(INADDR_ANY);
      } else {
        printf("inet_pton(AF_INET, %s, &addr.sin_addr);\n", ip_str);
        if(!inet_pton(AF_INET, ip_str, &addr.sin_addr)){
          printf("Wrong IP address\n");
          continue;
        }
      }

      // Print code
      printf("ret = bind(%d, (sockaddr*)&addr, sizeof(addr));\n", sock);

      // Bind the address with the socket, as a server
      // Document: https://man7.org/linux/man-pages/man2/bind.2.html
      int ret = bind(sock, (sockaddr*)&addr, sizeof(addr));
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      }
    } else if(strncmp(cmd, "sendto", 6) == 0) {
      if(sscanf(cmd, "sendto %d %s %s %d", &sock, buf, ip_str, &port) != 4){
        printf("Usage: sendto SOCKET STRING\n");
        continue;
      }

      // Print code
      printf("sockaddr_in addr;\n");
      printf("addr.sin_family = AF_INET;\n");
      printf("addr.sin_port = htons(%d);\n", port);
      printf("inet_pton(AF_INET, %s, &addr.sin_addr);\n", ip_str);

      // IPv4 family
      addr.sin_family = AF_INET;
      // Port number. Note that IP uses big-endian.
      addr.sin_port = htons(port);
      // Convert IP address from string to binary form
      if(!inet_pton(AF_INET, ip_str, &addr.sin_addr)){
        printf("Wrong IP address\n");
        continue;
      }

      // Print code
      printf("ret = sendto(%d, \"%s\", %lu, 0, (sockaddr*)&addr, sizeof(addr));\n", sock, buf, strlen(buf));

      int ret = sendto(sock, buf, strlen(buf), 0, (sockaddr*)&addr, sizeof(addr));
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      } else {
        printf("Sent %d bytes\n", ret);
      }
    } else if(strncmp(cmd, "recvfrom", 8) == 0) {
      if(sscanf(cmd, "recvfrom %d %d", &sock, &buflen) != 2){
        printf("Usage: recvfrom SOCKET BUFFER-LENGTH\n");
        continue;
      }
      if(buflen < 0){
        buflen = 0;
      } else if (buflen > (int)sizeof(buf)) {
        buflen = sizeof(buf);
      }

      // Print code
      printf("socklen = sizeof(addr);\n");
      printf("ret = recvfrom(%d, buf, %d, 0, (sockaddr*)&addr, &socklen);\n", sock, buflen);

      socklen = sizeof(addr);
      int ret = recvfrom(sock, buf, buflen, 0, (sockaddr*)&addr, &socklen);
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      } else {
        buf[ret] = 0;  // Adding a tailing '\0' for print
        printf("Recved %d bytes\n", ret);
        printf("buf: %s\n", buf);
        inet_ntop(AF_INET, &addr.sin_addr, ip_str, socklen);
        printf("Client IP address: %s port: %d\n", ip_str, ntohs(addr.sin_port));
      }
    } else if(strncmp(cmd, "send", 4) == 0) {
      if(sscanf(cmd, "send %d %s", &sock, buf) != 2){
        printf("Usage: send SOCKET STRING\n");
        continue;
      }

      // Print code
      printf("ret = send(%d, \"%s\", %lu, 0);\n", sock, buf, strlen(buf));

      // Send data to the socket (you may also use write)
      // Document: https://man7.org/linux/man-pages/man2/send.2.html
      // Length is strlen(buf)
      // The last argument is flag, can be ignored for now
      int ret = send(sock, buf, strlen(buf), 0);
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      } else {
        printf("Sent %d bytes\n", ret);
      }
    } else if(strncmp(cmd, "recv", 4) == 0) {
      if(sscanf(cmd, "recv %d %d", &sock, &buflen) != 2){
        printf("Usage: recv SOCKET BUFFER-LENGTH\n");
        continue;
      }
      if(buflen < 0){
        buflen = 0;
      } else if (buflen > (int)sizeof(buf)) {
        buflen = sizeof(buf);
      }

      // Print code
      printf("ret = recv(%d, buf, %d, 0);\n", sock, buflen);

      // Receive data from the socket (you may also use read)
      // Document: https://man7.org/linux/man-pages/man2/recv.2.html
      // The last argument is flag, can be ignored for now
      int ret = recv(sock, buf, buflen, 0);
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      } else {
        buf[ret] = 0;  // Adding a tailing '\0' for print
        printf("Recved %d bytes\n", ret);
        printf("buf: %s\n", buf);
      }
    } else if(strncmp(cmd, "close", 5) == 0) {
      if(sscanf(cmd, "close %d", &sock) != 1){
        printf("Usage: close SOCKET\n");
        continue;
      }

      // Print code
      printf("ret = close(%d);\n", sock);

      // Shutdown socket
      int ret = close(sock);
      printf("ret == %d\n", ret);
      if(ret == -1){
        printf("Error with code %d: %s\n", errno, strerror(errno));
      }
      sock = -1;
    } else {
      printf("Unknown command: %s\n", cmd);
      printf("Supported: socket, bind, send, recv, sendto, recvfrom, close, exit\n");
    }
  }
  return 0;
}
