#include "packet.h"
#include "protocol.h"
#include <algorithm>
#include <arpa/inet.h>
#include <cmath>
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

    // Handshake process: send SYN packet; wait for SYN-ACK packet; send ACK packet (w/ payload)
    size_t cwnd = INIT_CWND, ssthresh = INIT_SSTHRESH;
    client_header.set_syn_flag();
    client_header.encode((uint8_t*) buf);
    if (sendto(sock, buf, HEADER_LENGTH, 0, (sockaddr*) &addr, socklen) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }
    output_client_send(client_header, cwnd);

    if (recvfrom(sock, buf, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &addr, &socklen) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return 1;
    }
    server_header.decode((uint8_t*) buf);
    output_client_recv(client_header, cwnd);

    if (!server_header.syn_flag() || !server_header.ack_flag() ||
        server_header.acknowledgement_number() != client_header.sequence_number() + 1) {
        std::cerr << "ERROR: invalid SYN-ACK packet received" << std::endl;
    }

    client_header.set_connection_id(server_header.connection_id());
    client_header.set_sequence_number(client_header.sequence_number() + 1);
    client_header.clear_flags();
    client_header.set_ack_flag();
    client_header.set_acknowledgement_number(server_header.sequence_number() + 1);

    size_t bytes_read, bytes_received;
    while (!ifs.eof()) {
        int n_sent = 0;
        size_t byteswnd_read = 0;

        // Send cwnd number of bytes in transmission round
        size_t wnd = std::min(cwnd, (size_t) RWND);
        while(!ifs.eof() && byteswnd_read < wnd) {
            client_header.encode((uint8_t*) buf);
            ifs.read(buf + HEADER_LENGTH, PAYLOAD_LENGTH);
            bytes_read = ifs.gcount();
            
            if (sendto(sock, buf, HEADER_LENGTH + bytes_read, 0, (sockaddr*) &addr, socklen) < 0) {
                std::cerr << "ERROR: " << strerror(errno) << std::endl;
                return 1;
            }

            output_client_send(client_header, cwnd);
            client_header.set_sequence_number(client_header.sequence_number() + bytes_read);
            
            byteswnd_read += bytes_read;
            n_sent++;
        }

        // Wait to receive ACKs from server for transmission round
        size_t cwnd_next = cwnd;
        while (n_sent > 0) {
            if ((bytes_received = recvfrom(sock, buf, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &addr, &socklen)) < 0) {
                std::cerr << "ERROR: " << strerror(errno) << std::endl;
                return 1;
            }
            if (bytes_received <= 0) {
                std::cerr << "ERROR: Missing ACK" << std::endl;
            }

            server_header.decode((uint8_t*) buf);
            output_client_recv(client_header, cwnd);
            
            if (cwnd < ssthresh) {
                // Slow start phase
                cwnd_next += PAYLOAD_LENGTH;
            } else {
                // Congestion avoidance phase
                cwnd_next += (PAYLOAD_LENGTH * PAYLOAD_LENGTH) / cwnd;
            }

            n_sent--;
        }

        client_header.clear_flags();
        client_header.set_ack_flag();
        cwnd = cwnd_next;
    }
    ifs.close();

    // todo: shutdown handshake

    close(sock);
    return 0;
}
