#include "client_connection.h"
#include "packet.h"

ClientConnection::ClientConnection(const PacketHeader& syn_packet, uint16_t connection_id, const std::string& directory, int socket,
                                   sockaddr_in client_address)
    : m_connection_id(connection_id), m_client_address(client_address), m_socket(socket) {
    m_acknowledgement_number = syn_packet.sequence_number() + 1;

    auto path = directory + "/" + std::to_string(m_connection_id) + ".file";
    m_stream = std::ofstream { path };

    update_last_received_time();

    output_server_recv(syn_packet);

    m_pending_syn = true;
    send_ack(false);
}

ClientConnection::ReceiveAction ClientConnection::receive_packet(const PacketHeader& header, const uint8_t* payload,
                                                                 size_t payload_length) {
    output_server_recv(header);

    update_last_received_time();

    if (header.ack_flag() && header.acknowledgement_number() != m_sequence_number) {
        if ((!pending_syn() && !pending_fin()) || header.acknowledgement_number() != m_sequence_number + 1) {
            std::cerr << "ERROR: Incoming ACK has nonsense acknowledgement number" << std::endl;
        } else if (pending_syn()) {
            m_pending_syn = false;
            m_sequence_number += 1;
        } else if (pending_fin()) {
            m_pending_fin = false;
            m_stream.close();
            return ReceiveAction::Close;
        }
    }

    // Clamp the data payload so that it fits in the current window.
    auto sequence_number = SequenceNumber { header.sequence_number() };
    if (sequence_number + payload_length > m_acknowledgement_number + RWND) {
        if (sequence_number > m_acknowledgement_number + RWND) {
            payload_length = 0;
        } else {
            payload_length = (m_acknowledgement_number + RWND).difference(sequence_number);
        }
    }

    if (header.fin_flag()) {
        m_pending_packets.push({ header.sequence_number(), {}, true });
    } else if (payload_length != 0 && sequence_number + payload_length > m_acknowledgement_number) {
        m_pending_packets.push({ header.sequence_number(), std::vector<uint8_t>(payload, payload + payload_length), header.fin_flag() });
    }

    auto original_acknowledgement_number = m_acknowledgement_number;
    while (!m_pending_packets.empty() && m_pending_packets.top().sequence_number <= m_acknowledgement_number) {
        auto& packet = m_pending_packets.top();
        if (packet.is_fin && packet.sequence_number == m_acknowledgement_number) {
            m_acknowledgement_number += 1;
            m_pending_fin = true;
        } else if (packet.sequence_number + packet.data.size() > m_acknowledgement_number) {
            auto relevant_length = (packet.sequence_number + packet.data.size()).difference(m_acknowledgement_number);
            if (relevant_length > 0) {
                receive_data(packet.data.data() + (packet.data.size() - relevant_length), relevant_length);
                m_acknowledgement_number += relevant_length;
            }
        }

        m_pending_packets.pop();
    }

    bool is_dup = original_acknowledgement_number == m_acknowledgement_number;
    send_ack(is_dup);
    return ReceiveAction::KeepOpen;
}

void ClientConnection::receive_data(const uint8_t* data, size_t length) {
    m_stream.write((const char*) data, length);
    m_stream.flush();
}

void ClientConnection::update_last_received_time() {
    m_last_received = std::chrono::system_clock::now();
}

void ClientConnection::send_ack(bool is_dup) {
    PacketHeader header;
    header.set_sequence_number(m_sequence_number);
    header.set_acknowledgement_number(m_acknowledgement_number);
    header.set_connection_id(m_connection_id);
    header.set_ack_flag();
    if (pending_syn()) {
        header.set_syn_flag();
    }
    if (pending_fin()) {
        header.set_fin_flag();
    }

    uint8_t buffer[HEADER_LENGTH];
    header.encode(buffer);

    output_server_send(header, is_dup);
    sendto(m_socket, buffer, sizeof(buffer), 0, (sockaddr*) &m_client_address, sizeof(m_client_address));
}
