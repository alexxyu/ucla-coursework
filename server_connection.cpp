#include "server_connection.h"
#include "packet.h"
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
        std::cerr << "ERROR init: " << strerror(errno) << std::endl;
    }
    output_client_send(client_header, m_cwnd);

    socklen_t socklen = sizeof(m_server_address);
    if (recvfrom(m_socket, buffer, HEADER_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen) < 0) {
        std::cerr << "ERROR rec: " << strerror(errno) << std::endl;
    }
    server_header.decode((uint8_t*) buffer);
    output_client_recv(client_header, m_cwnd);

    if (!server_header.syn_flag() || !server_header.ack_flag() ||
        server_header.acknowledgement_number() != client_header.sequence_number() + 1) {
        std::cerr << "ERROR: invalid SYN-ACK packet received" << std::endl;
    }

    m_connection_id = server_header.connection_id();
    m_acknowledgement_number = server_header.sequence_number() + 1;
    m_sequence_number = m_sequence_number + 1;
}

void ServerConnection::send_data() {
    size_t bytes_received;
    socklen_t socklen = sizeof(m_server_address);
    while (!m_stream.eof()) {
        size_t n_sent = send_transmission_round();
        
        // Wait to receive ACKs from server for transmission round
        size_t cwnd_next = m_cwnd;
        char buffer[HEADER_LENGTH];
        while (n_sent > 0) {
            PacketHeader server_header;
            if ((bytes_received = recvfrom(m_socket, buffer, PACKET_LENGTH, MSG_WAITALL, (sockaddr*) &m_server_address, &socklen)) < 0) {
                std::cerr << "ERROR: " << strerror(errno) << std::endl;
            }
            if (bytes_received <= 0) {
                std::cerr << "ERROR: Missing ACK" << std::endl;
            }

            server_header.decode((uint8_t*) buffer);
            output_client_recv(server_header, m_cwnd);

            if (m_cwnd < m_ssthresh) {
                // Slow start phase
                cwnd_next += PAYLOAD_LENGTH;
            } else {
                // Congestion avoidance phase
                cwnd_next += (PAYLOAD_LENGTH * PAYLOAD_LENGTH) / m_cwnd;
            }

            n_sent--;
        }

        m_cwnd = cwnd_next;
    }

    // m_sequence_number = header.acknowledgement_number();
    // m_acknowledgement_number = header.sequence_number() + payload_length;

    // send_ack(false, header.fin_flag());
}

int ServerConnection::send_transmission_round() {
    char buffer[PACKET_LENGTH];
    
    int n_sent = 0;
    size_t byteswnd_read = 0;
    size_t wnd = std::min(m_cwnd, (size_t) RWND);

    size_t bytes_read;
    while(!m_stream.eof() && byteswnd_read < wnd) {
        PacketHeader header;
        header.set_sequence_number(m_sequence_number);
        header.set_acknowledgement_number(m_acknowledgement_number);
        header.set_connection_id(m_connection_id);
        header.set_ack_flag();

        header.encode((uint8_t*) buffer);
        m_stream.read(buffer + HEADER_LENGTH, PAYLOAD_LENGTH);
        bytes_read = m_stream.gcount();
        
        if (sendto(m_socket, buffer, HEADER_LENGTH + bytes_read, 0, (sockaddr*) &m_server_address, sizeof(m_server_address)) < 0) {
            std::cerr << "ERROR: " << strerror(errno) << std::endl;
            return 1;
        }

        output_client_send(header, m_cwnd);
        header.set_sequence_number(header.sequence_number() + bytes_read);
        
        byteswnd_read += bytes_read;
        n_sent++;
        m_sequence_number += bytes_read;
    }

    return n_sent;
}

void ServerConnection::close_connection() {
    m_stream.close();

    // todo: shutdown handshake
}
