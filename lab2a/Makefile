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

tests:
	echo "run all (over 200) specified test cases to generate results in CSV files. Note that the lab2_list program is expected to fail when running multiple threads without synchronization. Make sure that your Makefile continues executing despite such failures (e.g. put a '-' in front of commands that are expected to fail)."

graphs:
	echo "use gnuplot(1) and the supplied data reduction scripts to generate the required graphs"

dist: $(DISTNAME)
dist-files = lab1b-client.c lab1b-server.c Makefile README
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files)

clean:
	rm -rf *.o *.$(TAREXT) lab2_add lab2_list