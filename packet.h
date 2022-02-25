#pragma once

#include <arpa/inet.h>
#include <iostream>
#include <stdint.h>

#define HEADER_LENGTH  12
#define PAYLOAD_LENGTH 512
#define PACKET_LENGTH  HEADER_LENGTH + PAYLOAD_LENGTH

class [[gnu::packed]] PacketHeader {
public:
    const static uint16_t FIN_FLAG = (1 << 0);
    const static uint16_t SYN_FLAG = (1 << 1);
    const static uint16_t ACK_FLAG = (1 << 2);

    uint32_t sequence_number() const { return ntohl(m_sequence_number); }
    uint32_t acknowledgement_number() const { return ntohl(m_acknowledgement_number); }
    uint16_t connection_id() const { return ntohs(m_connection_id); }

    void set_sequence_number(uint32_t sequence_number) { m_sequence_number = htonl(sequence_number); }
    void set_acknowledgement_number(uint32_t acknowledgement_number) { m_acknowledgement_number = htonl(acknowledgement_number); }
    void set_connection_id(uint16_t connection_id) { m_connection_id = htons(connection_id); }

    bool fin_flag() const { return !!(m_flags & FIN_FLAG); }
    bool syn_flag() const { return !!(m_flags & SYN_FLAG); }
    bool ack_flag() const { return !!(m_flags & ACK_FLAG); }

    void set_fin_flag() { m_flags |= FIN_FLAG; }
    void set_syn_flag() { m_flags |= SYN_FLAG; }
    void set_ack_flag() { m_flags |= ACK_FLAG; }
    void clear_flags() { m_flags = 0; }

    void encode(uint8_t* buf) {
        buf[0] = (m_sequence_number >> 24) & 0xff;
        buf[1] = (m_sequence_number >> 16) & 0xff;
        buf[2] = (m_sequence_number >> 8) & 0xff;
        buf[3] = (m_sequence_number & 0xff);

        buf[4] = (m_acknowledgement_number >> 24) & 0xff;
        buf[5] = (m_acknowledgement_number >> 16) & 0xff;
        buf[6] = (m_acknowledgement_number >> 8) & 0xff;
        buf[7] = (m_acknowledgement_number & 0xff);

        buf[8] = (m_connection_id >> 8) & 0xff;
        buf[9] = (m_connection_id & 0xff);

        buf[10] = m_reserved;
        buf[11] = m_flags;
    }

    void decode(uint8_t* buf) {
        m_sequence_number = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
        m_acknowledgement_number = (buf[4] << 24) | (buf[5] << 16) | (buf[6] << 8) | buf[7];
        m_connection_id = (buf[8] << 8) | buf[9];
        m_flags = buf[11];
    }

private:
    uint32_t m_sequence_number { 0 };
    uint32_t m_acknowledgement_number { 0 };
    uint16_t m_connection_id { 0 };
    uint8_t m_reserved { 0 };
    uint8_t m_flags { 0 };
};

// Packet is defined to be 12 bytes
static_assert(sizeof(PacketHeader) == HEADER_LENGTH, "Packet header length is incorrect.");

inline static void output_server_recv(const PacketHeader& header) {
    std::cout << "RECV " << header.sequence_number() << " " << header.acknowledgement_number() << " " << header.connection_id();
    if (header.ack_flag()) {
        std::cout << " ACK";
    }
    if (header.syn_flag()) {
        std::cout << " SYN";
    }
    if (header.fin_flag()) {
        std::cout << " FIN";
    }
    std::cout << std::endl;
}

inline static void output_server_drop(const PacketHeader& header) {
    std::cout << "DROP " << header.sequence_number() << " " << header.acknowledgement_number() << " " << header.connection_id();
    if (header.ack_flag()) {
        std::cout << " ACK";
    }
    if (header.syn_flag()) {
        std::cout << " SYN";
    }
    if (header.fin_flag()) {
        std::cout << " FIN";
    }
    std::cout << std::endl;
}

inline static void output_server_send(const PacketHeader& header) {
    std::cout << "SEND " << header.sequence_number() << " " << header.acknowledgement_number() << " " << header.connection_id();
    if (header.ack_flag()) {
        std::cout << " ACK";
    }
    if (header.syn_flag()) {
        std::cout << " SYN";
    }
    if (header.fin_flag()) {
        std::cout << " FIN";
    }
    // NOTE: server will never send duplicates.
    std::cout << std::endl;
}
