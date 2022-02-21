#include <arpa/inet.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <sys/socket.h>
#include <thread>
#include <unistd.h>

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <PORT> <FILE-DIR>" << std::endl;
        return 1;
    }

    sockaddr_in addr;
    socklen_t socklen;

    int port = atoi(argv[1]);
    char* filedir = argv[2];

    int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    socklen = sizeof(addr);

    if (bind(sock, (sockaddr*) &addr, socklen) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    std::stringstream filepath;
    filepath << filedir << "/"
             << "1.file";
    std::ofstream ofs(filepath.str());

    int bytes;
    char buf[512];
    while ((bytes = recvfrom(sock, buf, 512, MSG_WAITALL, (sockaddr*) &addr, &socklen)) >= 512) {
        ofs.write(buf, bytes);
    }
    ofs.write(buf, bytes);
    ofs.close();

    char client_ip[256];
    inet_ntop(AF_INET, &addr.sin_addr, client_ip, socklen);
    std::cerr << "Client IP address: " << client_ip << " port: " << ntohs(addr.sin_port) << std::endl;

    close(sock);
    return 0;
}
