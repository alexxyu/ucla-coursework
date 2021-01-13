# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall
OBJS = lab1a.o

TARGET = lab1a

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab1a-105295708.$(TAREXT)

default: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

lab1a.o: lab1a.c
	$(CC) $(CFLAGS) -c lab1a.c

dist: $(DISTNAME)
dist-files = lab1a.c Makefile
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) $(OBJS) $(TARGET)
