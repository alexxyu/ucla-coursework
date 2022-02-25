#include "client_connection.h"
#include "packet.h"
#include "protocol.h"
#include <arpa/inet.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <sys/socket.h>
#include <thread>
#include <unistd.h>
#include <unordered_map>

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

    std::unordered_map<uint16_t, ClientConnection> connections;
    uint16_t next_connection_id = 1;

    uint8_t buf[PACKET_LENGTH];
    ssize_t bytes_received;
    while ((bytes_received = recvfrom(sock, buf, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &addr, &socklen)) > 0) {
        auto now = std::chrono::system_clock::now();
        for (auto it = connections.begin(); it != connections.end();) {
            auto next = it;
            next++;
            if (now >= it->second.expiraction_time()) {
                connections.erase(it);
            }
            it = next;
        }

        if (bytes_received < HEADER_LENGTH) {
            std::cerr << "ERROR: packet length (" << bytes_received << " bytes) is too small" << std::endl;
            continue;
        }

        PacketHeader header;
        header.decode(buf);

        const uint8_t* payload = buf + HEADER_LENGTH;
        size_t payload_length = bytes_received - HEADER_LENGTH;

        if (header.syn_flag()) {
            auto connection_id = next_connection_id++;
            connections.emplace(std::piecewise_construct, std::forward_as_tuple(connection_id),
                                std::forward_as_tuple(header, connection_id, filedir, sock, addr));
        } else {
            auto it = connections.find(header.connection_id());
            if (it == connections.end()) {
                output_server_drop(header);
            } else {
                it->second.receive_packet(header, payload, payload_length);
            }
        }
    }
    if (bytes_received < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    close(sock);
    return 0;
}
