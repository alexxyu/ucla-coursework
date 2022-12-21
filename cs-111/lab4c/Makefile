# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall -lm

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab4c-105295708.$(TAREXT)

default: lab4c_tcp lab4c_tls

lab4c_tcp:
	if [ `uname -a | grep -c "armv7l"` -ne 0 ]; then \
		$(CC) $(CFLAGS) -lmraa $@.c -o $@ ; \
	else \
		$(CC) $(CFLAGS) -DDUMMY $@.c -o $@ ; \
	fi

lab4c_tls:
	if [ `uname -a | grep -c "armv7l"` -ne 0 ]; then \
		$(CC) $(CFLAGS) -lmraa -lssl -lcrypto $@.c -o $@ ; \
	else \
		$(CC) $(CFLAGS) -DDUMMY -lssl -lcrypto $@.c -o $@ ; \
	fi

dist: $(DISTNAME)
dist-files = lab4c_tcp.c lab4c_tls.c Makefile README
$(DISTNAME): 
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) lab4c_tcp lab4c_tls