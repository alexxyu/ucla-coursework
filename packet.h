#pragma once

#include <arpa/inet.h>
#include <stdint.h>

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

private:
    uint32_t m_sequence_number { 0 };
    uint32_t m_acknowledgement_number { 0 };
    uint16_t m_connection_id { 0 };
    uint8_t m_reserved { 0 };
    uint8_t m_flags { 0 };
};

// Packet is definied to be 12 bytes
static_assert(sizeof(PacketHeader) == 12);
