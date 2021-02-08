# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -g -Wextra -Wall

TAR = tar
TARFLAGS = -czvf
TAREXT = tar.gz
DISTNAME = lab2b-105295708.$(TAREXT)

ifndef VERBOSE
.SILENT:
endif

default: lab2_list

lab2_list:
	$(CC) $(CFLAGS) -pthread lab2_list.c SortedList.c -o $@

tests: lab2_list
	./test.sh

graphs:
	./lab2_list.gp

profile: lab2_list
	rm -rf profile.out
	LD_PRELOAD=/usr/lib64/libprofiler.so.0 CPUPROFILE=./raw.gperf ./lab2_list --threads=12 --iterations=1000 --sync=s
	pprof --text ./lab2_list ./raw.gperf >> profile.out
	pprof --list=run ./lab2_list ./raw.gperf >> profile.out
	rm raw.gperf

dist:  $(DISTNAME)
$(DISTNAME): lab2_list
	if [ ! -f lab2b_list.csv ]; then \
		./test.sh; \
	fi; \
	if [ ! -f lab2b_1.png ] || [ ! -f lab2b_2.png ] || [ ! -f lab2b_3.png ] \
	   [ ! -f lab2b_4.png ] || [ ! -f lab2b_5.png ]; then \
		./lab2_list.gp; \
	fi; \
	if [ ! -f profile.out ]; then \
		rm -rf profile.out; \
		LD_PRELOAD=/usr/lib64/libprofiler.so.0 CPUPROFILE=./raw.gperf ./lab2_list --threads=12 --iterations=1000 --sync=s; \
		pprof --text ./lab2_list ./raw.gperf >> profile.out; \
		pprof --list=run ./lab2_list ./raw.gperf >> profile.out; \
		rm raw.gperf; \
	fi; \
	rm -rf *.o lab2_list
	$(TAR) $(TARFLAGS) $@ SortedList.h SortedList.c lab2_list.c Makefile test.sh README lab2_list.gp *.csv *.png profile.out

clean:
	rm -rf *.o *.$(TAREXT) lab2_list