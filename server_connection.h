#pragma once

#include <chrono>
#include <fstream>
#include <list>

#include "packet.h"
#include "protocol.h"

class Packet {
public:
    Packet(const PacketHeader& header, uint8_t* buf, size_t len) : m_header(header) {
        memcpy(m_payload, buf, len);
        m_payload_len = len;
    }

    PacketHeader header() const { return m_header; }
    size_t payload_len() const { return m_payload_len; }
    const uint8_t* payload() const { return m_payload; }

    bool is_retransmission() const { return m_is_retransmission; }
    void set_retransmission() { m_is_retransmission = true; }

private:
    PacketHeader m_header;
    size_t m_payload_len { 0 };
    bool m_is_retransmission { false };
    uint8_t m_payload[PAYLOAD_LENGTH];
};

class ServerConnection {
public:
    ServerConnection(const std::string& filename, int socket, sockaddr_in client_address);

    void init_connection();
    void send_data();
    void close_connection();

    uint16_t connection_id() const { return m_connection_id; }

private:
    int send_transmission_round();
    void wait_for_ACK(bool send_syn, bool send_fin);

    size_t m_cwnd { INIT_CWND };
    size_t m_ssthresh { INIT_SSTHRESH };
    uint16_t m_connection_id { 0 };
    std::ifstream m_stream;
    sockaddr_in m_server_address;
    int m_socket { -1 };
    SequenceNumber m_sequence_number { INIT_SEQNO_CLIENT };
    SequenceNumber m_acknowledgement_number { 0 };
    std::list<Packet> m_packets;
    size_t m_packet_bytes { 0 };
};
