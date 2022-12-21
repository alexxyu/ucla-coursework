# NAME: Alex Yu,Nicolas Trammer
# EMAIL: alexy23@g.ucla.edu,colet@g.ucla.edu
# ID: 105295708,005395690

CC = gcc
CFLAGS = -Wextra -Wall

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab3a-105295708.$(TAREXT)

TARGET = lab3a

default: $(TARGET)

$(TARGET): lab3a.c
	$(CC) $(CFLAGS) lab3a.c -o $@

dist: $(DISTNAME)
dist-files = lab3a.c Makefile README ext2_fs.h
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) $(TARGET)
