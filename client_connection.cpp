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
    send_ack(true, false);
}

void ClientConnection::receive_packet(const PacketHeader& header, const uint8_t* payload, size_t payload_length) {
    output_server_recv(header);

    update_last_received_time();

    // NOTE: these fields all need to be validated, including sending the fin flag later:
    // Also this assumes no packet loss and perfectly in order data.
    m_sequence_number = header.acknowledgement_number();
    m_acknowledgement_number = header.sequence_number() + payload_length;

    send_ack(false, header.fin_flag());

    receive_data(payload, payload_length);
}

void ClientConnection::receive_data(const uint8_t* data, size_t length) {
    m_stream.write((const char*) data, length);
    m_stream.flush();
}

void ClientConnection::update_last_received_time() {
    m_last_received = std::chrono::system_clock::now();
}

void ClientConnection::send_ack(bool send_syn, bool send_fin) {
    PacketHeader header;
    header.set_sequence_number(m_sequence_number);
    header.set_acknowledgement_number(m_acknowledgement_number);
    header.set_connection_id(m_connection_id);
    header.set_ack_flag();
    if (send_syn) {
        header.set_syn_flag();
    }
    if (send_fin) {
        header.set_fin_flag();
    }

    uint8_t buffer[HEADER_LENGTH];
    header.encode(buffer);

    output_server_send(header);
    sendto(m_socket, buffer, sizeof(buffer), 0, (sockaddr*) &m_client_address, sizeof(m_client_address));
}
