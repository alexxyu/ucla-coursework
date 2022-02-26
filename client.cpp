#include "packet.h"
#include "protocol.h"
#include "server_connection.h"
#include <algorithm>
#include <arpa/inet.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <netdb.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <thread>
#include <unistd.h>

int main(int argc, char* argv[]) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <HOSTNAME-OR-IP> <PORT> <FILENAME>" << std::endl;
        return 1;
    }

    addrinfo hints = { 0 };
    hints.ai_family = AF_INET;

    addrinfo* result;
    int error = getaddrinfo(argv[1], argv[2], &hints, &result);
    if (error) {
        std::cerr << "ERROR: failed to resolve address/port: " << gai_strerror(error) << std::endl;
        return 1;
    }

    const char* filename = argv[3];
    int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    ServerConnection sc(filename, sock, *(sockaddr_in*) result->ai_addr);
    sc.init_connection();
    sc.send_data();
    sc.close_connection();

    close(sock);
    return 0;
}
