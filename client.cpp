#include "packet.h"
#include "protocol.h"
#include "server_connection.h"
#include <algorithm>
#include <arpa/inet.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sys/select.h>
#include <sys/socket.h>
#include <thread>
#include <unistd.h>

int main(int argc, char* argv[]) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <HOSTNAME-OR-IP> <PORT> <FILENAME>" << std::endl;
        return 1;
    }

    sockaddr_in addr;

    // TODO: resolve hostname / ip
    // char* ip_str = argv[1];
    int port = atoi(argv[2]);
    const char* filename = argv[3];

    int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = inet_addr("127.0.0.1");

    ServerConnection sc(filename, sock, addr);
    sc.init_connection();
    sc.send_data();
    sc.close_connection();

    close(sock);
    return 0;
}
