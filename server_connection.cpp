#include "server_connection.h"
#include "packet.h"
#include <chrono>
#include <cmath>
#include <cstring>
#include <stdio.h>
#include <unistd.h>

ServerConnection::ServerConnection(const std::string& filename, int socket, sockaddr_in server_address)
    : m_server_address(server_address), m_socket(socket) {
    m_stream = std::ifstream { filename };
}

void ServerConnection::init_connection() {
    PacketHeader client_header, server_header;
    uint8_t buffer[HEADER_LENGTH];

    // Send initial SYN packet to the server
    client_header.set_syn_flag();
    client_header.set_sequence_number(m_sequence_number);
    client_header.encode(buffer);
    if (sendto(m_socket, buffer, HEADER_LENGTH, 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return;
    }
    output_client_send(client_header, m_cwnd, m_ssthresh, false);

    // Expect to receive a SYN-ACK packet back from the server
    socklen_t socklen = sizeof(m_server_address);
    alarm(KEEPALIVE_TIMEOUT);
    if (recvfrom(m_socket, buffer, HEADER_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return;
    }
    server_header.decode(buffer);
    output_client_recv(server_header, m_cwnd, m_ssthresh);

    if (!server_header.syn_flag() || !server_header.ack_flag() ||
        server_header.acknowledgement_number() != client_header.sequence_number() + 1) {
        std::cerr << "ERROR: invalid SYN-ACK packet received" << std::endl;
    }

    m_connection_id = server_header.connection_id();
    m_acknowledgement_number = server_header.sequence_number() + 1;
    m_sequence_number = m_sequence_number + 1;
}

void ServerConnection::send_data() {
    uint8_t buffer[PACKET_LENGTH];
    socklen_t socklen = sizeof(m_server_address);

    timeval rto;
    double rto_intpart;
    rto.tv_usec = std::modf(RETRANSMISSION_TIMEOUT, &rto_intpart) * 1000000UL;
    rto.tv_sec = rto_intpart;
    setsockopt(m_socket, SOL_SOCKET, SO_RCVTIMEO, (struct timeval*) &rto, sizeof(struct timeval));

    ssize_t bytes_received;
    alarm(KEEPALIVE_TIMEOUT);
    while (!m_stream.eof() || !m_packets.empty()) {
        ssize_t n_sent = send_transmission_round();

        // Wait to receive ACKs from server for the transmission round
        size_t cwnd_next = m_cwnd;
        while (n_sent > 0) {
            if ((bytes_received = recvfrom(m_socket, buffer, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen)) <= 0) {
                if (errno == EWOULDBLOCK || errno == EAGAIN) {
                    // Handle case where retransmission timer has expired
                    m_ssthresh = cwnd_next / 2;
                    cwnd_next = INIT_CWND;
                    break;
                } else {
                    // Handle other errors
                    std::cerr << "ERROR: " << strerror(errno) << std::endl;
                    return;
                }
            } else {
                PacketHeader server_header;
                server_header.decode(buffer);

                if (server_header.ack_flag()) {
                    if (cwnd_next < m_ssthresh) {
                        // Slow start phase
                        cwnd_next += PAYLOAD_LENGTH;
                        cwnd_next = std::min(cwnd_next, (size_t) MAX_CWND);
                    } else {
                        // Congestion avoidance phase
                        cwnd_next += (PAYLOAD_LENGTH * PAYLOAD_LENGTH) / m_cwnd;
                        cwnd_next = std::min(cwnd_next, (size_t) MAX_CWND);
                    }

                    SequenceNumber ack = server_header.acknowledgement_number();
                    while (!m_packets.empty() && ack >= m_packets.front().header().sequence_number() + m_packets.front().payload_len()) {
                        m_bytes_queued -= m_packets.front().payload_len();
                        m_packets.pop_front();
                    }
                }

                output_client_recv(server_header, cwnd_next, m_ssthresh);
                alarm(KEEPALIVE_TIMEOUT);
            }
            n_sent--;
        }

        m_cwnd = std::min(cwnd_next, (size_t) MAX_CWND);
    }
}

int ServerConnection::send_transmission_round() {
    int n_sent = 0;
    size_t wnd = std::min(m_cwnd, (size_t) RWND);

    // Read from file and create more packets only if the total bytes in the list of sent packets
    // does not already exceed the window size
    ssize_t bytes_read;
    uint8_t buffer[PACKET_LENGTH];
    while (!m_stream.eof() && m_bytes_queued < wnd) {
        PacketHeader header;
        header.set_sequence_number(m_sequence_number);
        header.set_connection_id(m_connection_id);

        if (m_sequence_number == INIT_SEQNO_CLIENT + 1) {
            // Only set ACK flag and field if it's the first packet with data sent out
            header.set_acknowledgement_number(m_acknowledgement_number);
            header.set_ack_flag();
        }

        header.encode(buffer);

        m_stream.read((char*) buffer + HEADER_LENGTH, PAYLOAD_LENGTH);
        bytes_read = m_stream.gcount();

        if (bytes_read > 0) {
            Packet p(header, buffer + HEADER_LENGTH, bytes_read);
            m_bytes_queued += bytes_read;
            m_packets.push_back(p);

            m_sequence_number += bytes_read;
        }
    }

    // Send as many packets as the window size currently allows
    size_t bytes_sent = 0;
    for (Packet& p : m_packets) {
        bytes_sent += p.payload_len();
        if (bytes_sent > wnd) {
            break;
        }

        p.header().encode(buffer);
        memcpy(buffer + HEADER_LENGTH, p.payload(), p.payload_len());

        if (sendto(m_socket, buffer, HEADER_LENGTH + p.payload_len(), 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
        }

        output_client_send(p.header(), m_cwnd, m_ssthresh, p.is_retransmission());
        p.set_retransmission();
        n_sent++;
    }

    return n_sent;
}

void ServerConnection::close_connection() {
    uint8_t buffer[HEADER_LENGTH];

    m_stream.close();

    PacketHeader client_header, server_header;
    client_header.set_sequence_number(m_sequence_number);
    client_header.set_connection_id(m_connection_id);
    client_header.set_fin_flag();
    client_header.encode(buffer);

    socklen_t socklen = sizeof(m_server_address);
    alarm(KEEPALIVE_TIMEOUT);
    while (1) {
        // Send FIN packet to the server
        if (sendto(m_socket, buffer, HEADER_LENGTH, 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
            return;
        }
        output_client_send(client_header, m_cwnd, m_ssthresh, false);

        // Expect to receive a [FIN-]ACK packet back from the server
        if (recvfrom(m_socket, buffer, HEADER_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen) < 0) {
            if (errno != EWOULDBLOCK && errno != EAGAIN) {
                std::cerr << "ERROR: " << strerror(errno) << std::endl;
                return;
            }
        } else {
            server_header.decode(buffer);

            if (server_header.ack_flag() && server_header.acknowledgement_number() == m_sequence_number + 1) {
                m_sequence_number += 1;
                output_client_recv(server_header, m_cwnd, m_ssthresh);
                break;
            }
        }
    }

    if (server_header.fin_flag() && server_header.sequence_number() == m_acknowledgement_number) {
        m_acknowledgement_number += 1;
        send_ack();
    }

    struct timeval read_timeout;
    read_timeout.tv_sec = 0;
    read_timeout.tv_usec = 100;
    setsockopt(m_socket, SOL_SOCKET, SO_RCVTIMEO, &read_timeout, sizeof(read_timeout));

    // Respond to any incoming FIN packets with ACKs until timeout
    for (auto start = std::chrono::steady_clock::now(), now = start; now < start + std::chrono::seconds { FIN_TIMEOUT };
         now = std::chrono::steady_clock::now()) {
        if (recvfrom(m_socket, buffer, HEADER_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen) <= 0) {
            if (errno != EWOULDBLOCK && errno != EAGAIN) {
                std::cerr << "ERROR: " << strerror(errno) << std::endl;
                return;
            }
        } else {
            server_header.decode(buffer);

            if (server_header.fin_flag() && server_header.sequence_number() + 1 == m_acknowledgement_number) {
                output_client_recv(server_header, m_cwnd, m_ssthresh);
                send_ack();
            }
        }
    }

    // Note: At this point, the connection is closed
}

void ServerConnection::send_ack() {
    uint8_t buffer[HEADER_LENGTH];

    PacketHeader ack_header;
    ack_header.set_sequence_number(m_sequence_number);
    ack_header.set_acknowledgement_number(m_acknowledgement_number);
    ack_header.set_connection_id(m_connection_id);
    ack_header.set_ack_flag();
    ack_header.encode(buffer);

    if (sendto(m_socket, buffer, HEADER_LENGTH, 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
        std::cerr << "ERROR: " << strerror(errno) << std::endl;
        return;
    }

    output_client_send(ack_header, m_cwnd, m_ssthresh, false);
}
