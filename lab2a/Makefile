# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab2a-105295708.$(TAREXT)

ifndef VERBOSE
.SILENT:
endif

default: build

build: lab2_add lab2_list

lab2_add:
	$(CC) $(CFLAGS) -pthread lab2_add.c -o $@

lab2_list:
	$(CC) $(CFLAGS) -pthread lab2_list.c SortedList.c -o $@

tests: build
	./test.sh

graphs:
	./lab2_add.gp
	./lab2_list.gp

dist: $(DISTNAME)
dist-files = lab2_add.c SortedList.h SortedList.c lab2_list.c Makefile test.sh README \
	lab2_add.gp lab2_list.gp
$(DISTNAME): build $(dist-files)
	if [ ! -f lab2_add.csv ] || [ ! -f lab2_list.csv ]; then \
		./test.sh; \
	fi; \
	if [ ! -f lab2_add-1.png ] || [ ! -f lab2_add-2.png ] || [ ! -f lab2_add-3.png ] || \
	   [ ! -f lab2_add-4.png ] || [ ! -f lab2_add-5.png ]; then \
		./lab2_add.gp; \
	fi; \
	if [ ! -f lab2_list-1.png ] || [ ! -f lab2_list-2.png ] || [ ! -f lab2_list-3.png ] || \
	   [ ! -f lab2_list-4.png ]; then \
		./lab2_list.gp; \
	fi; \
	rm -rf *.o lab2_add lab2_list
	$(TAR) $(TARFLAGS) $@ $(dist-files) *.csv *.png

clean:
	rm -rf *.o *.$(TAREXT) lab2_add lab2_list