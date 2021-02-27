# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall -lm -lmraa

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab4b-105295708.$(TAREXT)

TARGET = lab4b

default: $(TARGET)

$(TARGET):
	$(CC) $(CFLAGS) lab4b.c -o $@

check: $(TARGET)
	./test.sh

dist: $(DISTNAME)
dist-files = lab4b.c Makefile README test.sh
$(DISTNAME): 
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) $(TARGET)