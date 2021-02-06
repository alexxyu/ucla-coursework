# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab2b-105295708.$(TAREXT)

default: lab2_list

lab2_list:
	$(CC) $(CFLAGS) -pthread lab2_list.c SortedList.c -o $@

tests: lab2_list
	./test.sh

graphs:
	./lab2_list.gp

dist: tests graphs $(DISTNAME)
distonly: $(DISTNAME)
dist-files = SortedList.h SortedList.c lab2_list.c Makefile test.sh README lab2_list.gp
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) $@ $(dist-files) *.csv *.png

clean:
	rm -rf *.o *.$(TAREXT) lab2_list