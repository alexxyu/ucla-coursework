# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wall -Wextra
OBJS = lab0.o

TARGET = lab0

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab0-105295708.$(TAREXT)

default: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

lab0.o: lab0.c
	$(CC) $(CFLAGS) -c lab0.c

check: $(TARGET)
	./check.sh

dist: $(DISTNAME)
dist-files = lab0.c Makefile README backtrace.png breakpoint.png check.sh
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -f *.o *.$(TAREXT) $(TARGET)