#include <arpa/inet.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sys/socket.h>
#include <thread>
#include <unistd.h>

int main(int argc, char* argv[]) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <HOSTNAME-OR-IP> <PORT> <FILENAME>" << std::endl;
        return 1;
    }

    sockaddr_in addr;
    socklen_t socklen;

    // TODO: port, ip_str, filename
    char* ip_str = argv[1];
    int port = atoi(argv[2]);
    char* filename = argv[3];

    int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    socklen = sizeof(addr);

    char buf[512];
    std::ifstream ifs(filename);
    while (!ifs.eof()) {
        ifs.read(buf, 512);
        if (sendto(sock, buf, strlen(buf), 0, (sockaddr*) &addr, socklen) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
            return 1;
        }
        memset(buf, 0, 512);
    }
    ifs.close();

    close(sock);
    return 0;
}
