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

    PacketHeader server_header, client_header;
    server_header.set_sequence_number(INIT_SEQNO_SERVER);

    size_t bytes_received;
    char buf[PACKET_LENGTH];
    while ((bytes_received = recvfrom(sock, buf, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &addr, &socklen)) > 0) {
        client_header.decode((uint8_t*) buf);
        std::cerr << "RECV " << client_header.sequence_number() << " " << client_header.acknowledgement_number() << " "
                  << client_header.connection_id() << " " << client_header.ack_flag() << " " << client_header.syn_flag() << " "
                  << client_header.fin_flag() << std::endl;
        ofs.write(buf + HEADER_LENGTH, bytes_received - HEADER_LENGTH);

        server_header.set_acknowledgement_number(client_header.sequence_number() + bytes_received - HEADER_LENGTH);
        server_header.set_ack_flag();
        server_header.encode((uint8_t*) buf);
        if (sendto(sock, buf, HEADER_LENGTH, 0, (sockaddr*) &addr, socklen) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
            return 1;
        }
        std::cerr << "SEND " << server_header.sequence_number() << " " << server_header.acknowledgement_number() << " "
                  << server_header.connection_id() << " " << server_header.ack_flag() << " " << server_header.syn_flag() << " "
                  << server_header.fin_flag() << std::endl;

        if (bytes_received < PACKET_LENGTH) {
            break;
        }
    }
    ofs.close();

    if (bytes_received < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }

    char client_ip[256];
    inet_ntop(AF_INET, &addr.sin_addr, client_ip, socklen);
    std::cerr << "Client IP address: " << client_ip << " port: " << ntohs(addr.sin_port) << std::endl;

    close(sock);
    return 0;
}
