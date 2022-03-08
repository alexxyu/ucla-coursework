## Names and Contributions

Alex Yu (105295708)
Nicolas Trammer (005395690)

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

## Extra Libraries

None

## Tutorials

We consulted the linux man page for `getaddrinfo` (which includes example code), in order to
resolve addresses properly.
