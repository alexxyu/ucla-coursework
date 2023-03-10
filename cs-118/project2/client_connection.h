#pragma once

#include "packet.h"
#include "protocol.h"
#include "sequence_number.h"

#include <chrono>
#include <fstream>
#include <memory>
#include <queue>

class ClientConnection {
    struct Packet {
        SequenceNumber sequence_number;
        std::vector<uint8_t> data;
        bool is_fin;

        bool operator>(const Packet& other) const { return this->sequence_number > other.sequence_number; }
    };

public:
    ClientConnection(const PacketHeader& syn_packet, uint16_t connection_id, const std::string& directory, int socket,
                     sockaddr_in client_address);

    enum class ReceiveAction {
        KeepOpen,
        Close,
    };

    ReceiveAction receive_packet(const PacketHeader& header, const uint8_t* payload, size_t payload_length);

    uint16_t connection_id() const { return m_connection_id; }

    bool pending_syn() const { return m_pending_syn; }
    bool pending_fin() const { return m_pending_fin; }

    std::chrono::time_point<std::chrono::system_clock> expiraction_time() const {
        return m_last_received + std::chrono::seconds { KEEPALIVE_TIMEOUT };
    }

private:
    void send_ack(bool is_dup);
    void receive_data(const uint8_t* data, size_t length);
    void update_last_received_time();

    std::chrono::time_point<std::chrono::system_clock> m_last_received;
    uint16_t m_connection_id { 0 };
    std::ofstream m_stream;
    sockaddr_in m_client_address;
    int m_socket { -1 };
    bool m_pending_syn { false };
    bool m_pending_fin { false };
    SequenceNumber m_sequence_number { INIT_SEQNO_SERVER };
    SequenceNumber m_acknowledgement_number { 0 };
    std::priority_queue<Packet, std::vector<Packet>, std::greater<Packet>> m_pending_packets;
};
