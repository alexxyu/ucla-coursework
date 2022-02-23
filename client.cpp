#include "packet.h"
#include "protocol.h"
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

    // TODO: resolve hostname / ip
    // char* ip_str = argv[1];
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

    std::ifstream ifs(filename);

    PacketHeader client_header, server_header;
    client_header.set_sequence_number(INIT_SEQNO_CLIENT);

    char buf[PACKET_LENGTH];
    memset(buf, 0, PACKET_LENGTH);

    size_t bytes_read, bytes_received;
    while (!ifs.eof()) {
        client_header.encode((uint8_t*) buf);
        ifs.read(buf + HEADER_LENGTH, PAYLOAD_LENGTH);
        bytes_read = ifs.gcount();
        if (sendto(sock, buf, HEADER_LENGTH + bytes_read, 0, (sockaddr*) &addr, socklen) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
            return 1;
        }
        std::cerr << "SEND " << client_header.sequence_number() << " " << client_header.acknowledgement_number() << " "
                  << client_header.connection_id() << " " << client_header.ack_flag() << " " << client_header.syn_flag() << " "
                  << client_header.fin_flag() << std::endl;
        client_header.set_sequence_number(client_header.sequence_number() + bytes_read);

        if ((bytes_received = recvfrom(sock, buf, HEADER_LENGTH, MSG_WAITALL, (sockaddr*) &addr, &socklen)) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
            return 1;
        }

        server_header.decode((uint8_t*) buf);
        std::cerr << "RECV " << server_header.sequence_number() << " " << server_header.acknowledgement_number() << " "
                  << server_header.connection_id() << " " << server_header.ack_flag() << " " << server_header.syn_flag() << " "
                  << server_header.fin_flag() << std::endl;
        client_header.set_acknowledgement_number(server_header.sequence_number());
        client_header.set_ack_flag();
    }
    ifs.close();

    close(sock);
    return 0;
}
