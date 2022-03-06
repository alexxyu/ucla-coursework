#include "client_connection.h"
#include "packet.h"
#include "protocol.h"
#include <arpa/inet.h>
#include <cstring>
#include <fstream>
#include <iostream>
#include <netdb.h>
#include <signal.h>
#include <sstream>
#include <sys/socket.h>
#include <thread>
#include <unistd.h>
#include <unordered_map>

void quit(int signum) {
    exit(0);
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <PORT> <FILE-DIR>" << std::endl;
        return 1;
    }

    signal(SIGINT, quit);
    signal(SIGQUIT, quit);
    signal(SIGTERM, quit);

    addrinfo hints = { 0 };
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
    hints.ai_flags = AI_PASSIVE | AI_NUMERICSERV;

    addrinfo* result;
    int error = getaddrinfo(nullptr, argv[1], &hints, &result);
    if (error) {
        std::cerr << "ERROR: failed to resolve address/port: " << gai_strerror(error) << std::endl;
        return 1;
    }

    char* filedir = argv[2];

    int sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (sock < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    if (bind(sock, result->ai_addr, result->ai_addrlen) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    freeaddrinfo(result);

    std::unordered_map<uint16_t, ClientConnection> connections;
    uint16_t next_connection_id = 1;

    uint8_t buf[PACKET_LENGTH];
    ssize_t bytes_received;
    sockaddr_in addr;
    socklen_t socklen = sizeof(addr);
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
                auto action = it->second.receive_packet(header, payload, payload_length);
                if (action == ClientConnection::ReceiveAction::Close) {
                    connections.erase(it);
                }
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
