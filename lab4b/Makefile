# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall -lm

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab4b-105295708.$(TAREXT)

TARGET = lab4b

default: $(TARGET)

$(TARGET):
	if [ `uname -a | grep -c "lasr.cs.ucla.edu"` -ne 0 ] || [ `uname -a | grep -c "beaglebone"` -ne 0 ]; then \
		$(CC) $(CFLAGS) -lmraa lab4b.c -o $@ ; \
	else \
		$(CC) $(CFLAGS) -DDUMMY lab4b.c -o $@ ; \
	fi

check: $(TARGET)
	./test.sh

dist: $(DISTNAME)
dist-files = lab4b.c Makefile README test.sh
$(DISTNAME): 
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) $(TARGET)