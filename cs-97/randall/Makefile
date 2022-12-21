# Make x86-64 random byte generators.

# Copyright 2015, 2020 Paul Eggert

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

# Optimization level.  Change this -O2 to -Og or -O0 or whatever.
OPTIMIZE = -O2

# The C compiler and its options.
# TODO: NEED TO ADD -fanalyze OPTION TO CFLAGS
CC = gcc
CFLAGS = $(OPTIMIZE) -g3 -Wall -Wextra  \
	-march=native -mtune=native -mrdrnd
OBJS = randall.o rand64-hw.o rand64-sw.o rand64-swf.o output.o options.o

# The archiver command, its options and filename extension.
TAR = tar
TARFLAGS = --xz --transform 's,^,randall/,'
TAREXT = txz

default: randall

randall: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

randall.o: randall.c
	$(CC) $(CFLAGS) -c randall.c

rand64-hw.o: rand64-hw.c rand64-hw.h
	$(CC) $(CFLAGS) -c rand64-hw.c

rand64-sw.o: rand64-sw.c rand64-sw.h
	$(CC) $(CFLAGS) -c rand64-sw.c

rand64-swf.o: rand64-swf.c rand64-swf.h
	$(CC) $(CFLAGS) -c rand64-swf.c

output.o: output.c output.h
	$(CC) $(CFLAGS) -c output.c

options.o: options.c options.h
	$(CC) $(CFLAGS) -c options.c

assignment: randall-assignment.$(TAREXT)
assignment-files = COPYING Makefile randall.c
randall-assignment.$(TAREXT): $(assignment-files)
	$(TAR) $(TARFLAGS) -cf $@ $(assignment-files)

submission: randall-submission.$(TAREXT)
submission-files = $(assignment-files) \
  notes.txt options.c options.h output.c output.h \
	rand64-hw.c rand64-hw.h rand64-sw.c rand64-sw.h \
	rand64-swf.c rand64-swf.h
randall-submission.$(TAREXT): $(submission-files)
	$(TAR) $(TARFLAGS) -cf $@ $(submission-files)

.PHONY: default clean assignment submission

clean:
	rm -f *.o *.$(TAREXT) randall

check: randall
	./randall -i mrand48 5000 | wc -c
	./randall -i mrand48 5000 -o 4 | wc -c
	./randall -i /dev/urandom 5000 | wc -c

