# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab3a-105295708.$(TAREXT)

TARGET = lab3a

default: $(TARGET)

$(TARGET):
	$(CC) $(CFLAGS) lab3a.c -o $@

dist: $(DISTNAME)
dist-files = lab3a.c Makefile README ext2_fs.h
$(DISTNAME): 
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) $(TARGET)