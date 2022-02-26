#include "server_connection.h"
#include "packet.h"
#include <cmath>
#include <cstring>
#include <stdio.h>

ServerConnection::ServerConnection(const std::string& filename, int socket, sockaddr_in server_address)
    : m_server_address(server_address), m_socket(socket) {
    m_stream = std::ifstream { filename };
}

void ServerConnection::init_connection() {
    uint8_t buffer[HEADER_LENGTH];
    PacketHeader client_header, server_header;
    client_header.set_syn_flag();
    client_header.set_sequence_number(m_sequence_number);
    client_header.encode((uint8_t*) buffer);

    if (sendto(m_socket, buffer, HEADER_LENGTH, 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
    }
    output_client_send(client_header, m_cwnd, false);

    socklen_t socklen = sizeof(m_server_address);
    if (recvfrom(m_socket, buffer, HEADER_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
    }
    server_header.decode((uint8_t*) buffer);
    output_client_recv(server_header, m_cwnd);

    if (!server_header.syn_flag() || !server_header.ack_flag() ||
        server_header.acknowledgement_number() != client_header.sequence_number() + 1) {
        std::cerr << "ERROR: invalid SYN-ACK packet received" << std::endl;
    }

    m_connection_id = server_header.connection_id();
    m_acknowledgement_number = server_header.sequence_number() + 1;
    m_sequence_number = m_sequence_number + 1;
}

void ServerConnection::send_data() {
    timeval rto;
    double rto_intpart;
    rto.tv_usec = std::modf(RETRANSMISSION_TIMEOUT, &rto_intpart) * 1000000;
    rto.tv_sec = rto_intpart;

    setsockopt(m_socket, SOL_SOCKET, SO_RCVTIMEO, (struct timeval*) &rto, sizeof(struct timeval));

    ssize_t bytes_received;
    socklen_t socklen = sizeof(m_server_address);
    while (!m_stream.eof() || !m_packets.empty()) {
        send_transmission_round();

        // Wait to receive ACKs from server for transmission round
        size_t cwnd_next = m_cwnd;
        char buffer[PACKET_LENGTH];
        while (!m_packets.empty()) {
            if ((bytes_received = recvfrom(m_socket, buffer, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen)) <= 0) {
                if (errno == EWOULDBLOCK || errno == EAGAIN) {
                    // Retransmission timer has expired
                    cwnd_next = INIT_CWND;
                    m_ssthresh /= 2;
                    m_packets.front().set_retransmission();
                    break;
                } else {
                    std::cerr << "ERROR: " << errno << " " << strerror(errno) << std::endl;
                }
            } else {
                PacketHeader server_header;
                server_header.decode((uint8_t*) buffer);
                output_client_recv(server_header, m_cwnd);

                if (server_header.ack_flag()) {
                    if (m_cwnd < m_ssthresh) {
                        // Slow start phase
                        cwnd_next += PAYLOAD_LENGTH;
                    } else {
                        // Congestion avoidance phase
                        cwnd_next += (PAYLOAD_LENGTH * PAYLOAD_LENGTH) / m_cwnd;
                    }

                    SequenceNumber ack = server_header.acknowledgement_number();
                    while (!m_packets.empty() && ack >= m_packets.front().header().sequence_number() + m_packets.front().payload_len()) {
                        m_packet_bytes -= m_packets.front().payload_len();
                        m_packets.pop_front();
                    }
                }
            }
        }

        m_cwnd = cwnd_next;
    }
}

int ServerConnection::send_transmission_round() {
    int n_sent = 0;
    size_t wnd = std::min(m_cwnd, (size_t) RWND);

    size_t bytes_read;
    char buffer[PACKET_LENGTH];
    while (!m_stream.eof() && m_packet_bytes < wnd) {
        PacketHeader header;
        header.set_sequence_number(m_sequence_number);
        header.set_acknowledgement_number(m_acknowledgement_number);
        header.set_connection_id(m_connection_id);
        header.set_ack_flag();

        header.encode((uint8_t*) buffer);
        m_stream.read(buffer + HEADER_LENGTH, PAYLOAD_LENGTH);
        bytes_read = m_stream.gcount();

        Packet p(header, buffer + HEADER_LENGTH, bytes_read);
        m_packet_bytes += bytes_read;
        m_packets.push_back(p);

        m_sequence_number += bytes_read; // todo: modulo
    }

    size_t bytes_sent = 0;
    for (Packet& p : m_packets) {
        bytes_sent += p.payload_len();
        if(bytes_sent > wnd) {
            break;
        }

        p.header().encode((uint8_t*) buffer);
        memcpy(buffer + HEADER_LENGTH, p.payload(), p.payload_len());

        if (sendto(m_socket, buffer, HEADER_LENGTH + p.payload_len(), 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
        }

        output_client_send(p.header(), m_cwnd, p.is_retransmission());
    }

    return n_sent;
}

void ServerConnection::close_connection() {
    m_stream.close();

    // todo: shutdown handshake
}
