# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wall -Wextra
OBJS = lab0.o

TAR = tar
TARFLAGS = --xz
TAREXT = tar.gz
DISTNAME = lab0-105295708.$(TAREXT)

default: lab0

lab0: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

lab0.o: lab0.c
	$(CC) $(CFLAGS) -c lab0.c

check:
	echo "checking"

dist: $(DISTNAME)
dist-files = lab0.c Makefile README \
	# backtrace.png breakpoint.png
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) -cf $@ $(dist-files)

clean:
	rm -f *.o *.$(TAREXT) lab0