# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab2a-105295708.$(TAREXT)

default: build

build: lab2_add

lab2_add:
	$(CC) $(CFLAGS) lab2_add.c -o $@

lab2_list:
	$(CC) $(CFLAGS) lab2_list.c -o $@

tests: lab2_add
	./test.sh

graphs:
	./lab2_add.gp

dist: $(DISTNAME)
dist-files = lab1b-client.c lab1b-server.c Makefile README
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) lab2_add lab2_list *.png