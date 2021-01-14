# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall

SERVEROBJS = lab1b-server.o
CLIENTOBJS = lab1b-client.o

TARGET = lab1b

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab1b-105295708.$(TAREXT)

default: $(TARGET)

$(TARGET): $(SERVEROBJS) $(CLIENTOBJS)
	$(CC) $(CFLAGS) $(SERVEROBJS) $(CLIENTOBJS) -o $@

server: $(SERVEROBJS)
	$(CC) $(CFLAGS) $(SERVEROBJS) -o $@

client: $(CLIENTOBJS)
	$(CC) $(CFLAGS) l$(CLIENTOBJS) -o $@

lab1b-server.o: lab1b-server.c
	$(CC) $(CFLAGS) -c lab1b-server.c

lab1b-client.o: lab1b-client.c
	$(CC) $(CFLAGS) -c lab1b-client.c

dist: $(DISTNAME)
dist-files = lab1b-client.c lab1b-server.c Makefile README
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) $(CLIENTOBJS) $(SERVEROBJS) $(TARGET)
