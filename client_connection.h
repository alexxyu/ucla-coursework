#pragma once

#include <chrono>
#include <fstream>

#include "packet.h"
#include "protocol.h"

class ClientConnection {
public:
    ClientConnection(const PacketHeader& syn_packet, uint16_t connection_id, const std::string& directory, int socket,
                     sockaddr_in client_address);

    void receive_packet(const PacketHeader& header, const uint8_t* payload, size_t payload_length);

    uint16_t connection_id() const { return m_connection_id; }

    std::chrono::time_point<std::chrono::system_clock> expiraction_time() const { return m_last_received + std::chrono::seconds { 10 }; }

private:
    void send_ack(bool send_syn, bool send_fin);
    void receive_data(const uint8_t* data, size_t length);
    void update_last_received_time();

    std::chrono::time_point<std::chrono::system_clock> m_last_received;
    uint16_t m_connection_id { 0 };
    std::ofstream m_stream;
    sockaddr_in m_client_address;
    int m_socket { -1 };
    uint32_t m_sequence_number { INIT_SEQNO_SERVER };
    uint32_t m_acknowledgement_number { 0 };
};
