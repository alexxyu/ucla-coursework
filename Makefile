CXX=g++
CXXOPTIMIZE= -O2
CXXFLAGS= -g -Wall -pthread -std=c++11 $(CXXOPTIMIZE)
USERID=005395690_ALEXID

SERVER_OBJ=\
client_connection.o \
server.o

CLIENT_OBJ=\
client.o

all: server client

%.o: %.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS)

server: $(SERVER_OBJ)
	$(CXX) -o $@ $^ $(CXXFLAGS)

client: $(CLIENT_OBJ)
	$(CXX) -o $@ $^ $(CXXFLAGS)

clean:
	rm -rf *.o *~ *.gch *.swp *.dSYM server client *.tar.gz

dist: tarball
tarball: clean
	tar -cvzf /tmp/$(USERID).tar.gz --exclude=./.vagrant . && mv /tmp/$(USERID).tar.gz .
