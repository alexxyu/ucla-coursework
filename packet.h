#pragma once

#include "sequence_number.h"
#include <arpa/inet.h>
#include <cstring>
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

    SequenceNumber sequence_number() const { return SequenceNumber { ntohl(m_sequence_number) }; }
    SequenceNumber acknowledgement_number() const { return SequenceNumber { ntohl(m_acknowledgement_number) }; }
    uint16_t connection_id() const { return ntohs(m_connection_id); }

    void set_sequence_number(SequenceNumber sequence_number) { m_sequence_number = htonl(sequence_number.raw_sequence_number()); }
    void set_acknowledgement_number(SequenceNumber acknowledgement_number) {
        m_acknowledgement_number = htonl(acknowledgement_number.raw_sequence_number());
    }
    void set_connection_id(uint16_t connection_id) { m_connection_id = htons(connection_id); }

    bool fin_flag() const { return !!(m_flags & FIN_FLAG); }
    bool syn_flag() const { return !!(m_flags & SYN_FLAG); }
    bool ack_flag() const { return !!(m_flags & ACK_FLAG); }

    void set_fin_flag() { m_flags |= FIN_FLAG; }
    void set_syn_flag() { m_flags |= SYN_FLAG; }
    void set_ack_flag() { m_flags |= ACK_FLAG; }
    void clear_flags() { m_flags = 0; }

    void encode(uint8_t* buf) { memcpy(buf, this, HEADER_LENGTH); }

    void decode(uint8_t* buf) { memcpy(this, buf, HEADER_LENGTH); }

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
    std::cout << "RECV " << header.sequence_number().raw_sequence_number() << " " << header.acknowledgement_number().raw_sequence_number()
              << " " << header.connection_id();
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
    std::cout << "DROP " << header.sequence_number().raw_sequence_number() << " " << header.acknowledgement_number().raw_sequence_number()
              << " " << header.connection_id();
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

inline static void output_server_send(const PacketHeader& header, bool duplicate) {
    std::cout << "SEND " << header.sequence_number().raw_sequence_number() << " " << header.acknowledgement_number().raw_sequence_number()
              << " " << header.connection_id();
    if (header.ack_flag()) {
        std::cout << " ACK";
    }
    if (header.syn_flag()) {
        std::cout << " SYN";
    }
    if (header.fin_flag()) {
        std::cout << " FIN";
    }
    if (duplicate) {
        std::cout << " DUP";
    }
    // NOTE: server will never send duplicates.
    std::cout << std::endl;
}

inline static void output_client_recv(const PacketHeader& header, size_t cwnd) {
    std::cout << "RECV " << header.sequence_number().raw_sequence_number() << " " << header.acknowledgement_number().raw_sequence_number()
              << " " << header.connection_id() << " " << cwnd;
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

inline static void output_client_send(const PacketHeader& header, size_t cwnd, bool is_dup) {
    std::cout << "SEND " << header.sequence_number().raw_sequence_number() << " " << header.acknowledgement_number().raw_sequence_number()
              << " " << header.connection_id() << " " << cwnd;
    if (header.ack_flag()) {
        std::cout << " ACK";
    }
    if (header.syn_flag()) {
        std::cout << " SYN";
    }
    if (header.fin_flag()) {
        std::cout << " FIN";
    }
    if (is_dup) {
        std::cout << " DUP";
    }
    std::cout << std::endl;
}
