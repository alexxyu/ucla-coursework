# CS118 Project 2

## Names and Contributions

Alex Yu (105295708) contributed primarily to the client implementation.
Nicolas Trammer (005395690) contributed primarily to the server implementation.

## Server Design

The high-level design is that the server creates a ClientConnection object for each persistent
connection that is setup. This object holds all the necessary state to receive data from a client,
and includes things like the next expected sequence number and the file output stream. The main()
function simply reads in packets in a loop, and uses a hash map to lookup the connection id of each
incoming packet. If it finds a connection, it forwards the packet to the ClientConnection object.
Otherwise, if the packet has the SYN flag, a new object is created, and if not, the packet is dropped.

Whenever the server receives valid (in-order) data, it just writes out the contents of the packet
to the output file. However, the server does buffer out-of-order packets which are within the
receive window. This implementation stores all (in-window) packets in a `std::priority_queue` keyed
by the incoming sequence number. The server then only processes the packets at the front of this
queue (earliest sequence numbers), and stops once a gap is encountered. Once the client fills in
this gap, more packets can be removed from this queue. If a packet is outside the window, it is
simply ignored.

## Client Design

The client makes use of a ServerConnection object that maintains connection state with the server
and the implementation of the client behavior. There are three phases to a client's connection: the
initial handshake, the sending of the actual file contents, and the closing handshake. We created a
method to handle each one of these stages. Each one is similar in design, so we will focus on the
method that handles file data transmission. 

The client maintains a queue of packets along with the number of bytes of data in the queue. It also
represents each packet as a Packet object, which maintains information about the packet's header,
whether it is a retransmission, and the payload. During each transmission round, the client attempts
to read more bytes from the file, creates packets, and adds them to a queue if the number of queued
bytes does not already exceed the window size. After sending out the packets, the client waits for
ACKs and removes every packet from the queue that has been acknowledged by the ACK's acknowledgement
number. Upon timeout, the client updates the congestion window size and retransmits the packet at
the front of the queue.

## Server Problems

One of the largest issues we encountered was properly handing the fact that sequence numbers wrap
around after reaching the value 102400. In the original implementation, this fact was simply
ignored, which meant it was impossible to properly transmit large files. We were able to fix
this by introducing a dedicated wrapper class: `SequenceNumber`. This class stores a single
uint32_t value, but wraps it in a way that ensures code deals with wrap around properly. Because
C++ supports operator overloading, and encourages its use especially with comparison operators,
we overloaded the compasison operators '<, <=, >, >=, ==, !=' to work 'correctly' given the
fact that sequence numbers wrap around. In practice, this means that the sequence number `1`
comes after `102400` but after `1000`. By using this wrapper class, the code written assuming
no wrap-around now worked properly. This allowed both the client and server to properly
transmit large files.

## Client Problems

The main problem we encountered when creating the client was figuring out how to buffer packets in
the case of retransmission. In this case, the client should not have to read from the file again but
rather resend the data it had already read. To this end, we decided to create a Packet class to
represent the packets that the client would send out, and we used a queue to manage the packets in
the current window. Thus, if packets were lost, the client could look to the queue to retrieve and
retransmit the necessary packets. A more minor complication that we ran into was whether the updated
cwnd value during congestion avoidance is based on the cwnd of the previous transmission round, or
the current cwnd as it is being updated.

## Extra Libraries

None

## Tutorials

We consulted the linux man pages for `getaddrinfo` and `setsockopt` (which includes example code),
in order to resolve addresses properly.
