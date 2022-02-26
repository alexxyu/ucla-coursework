#pragma once

#include <chrono>
#include <fstream>

#include "packet.h"
#include "protocol.h"

class Packet {
public:
    Packet(const PacketHeader& header, char* m_payload);
    Packet* next() const { return m_next; }

private:
    PacketHeader m_header;
    char* m_payload;
    Packet* m_next { nullptr };
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
    uint32_t m_sequence_number { INIT_SEQNO_CLIENT };
    uint32_t m_acknowledgement_number { 0 };
    Packet* m_packets_head { nullptr };
    Packet* m_packets_tail { nullptr };
};
