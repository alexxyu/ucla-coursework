# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wall -Wextra
OBJS = lab0.o

TARGET = lab0

TAR = tar
TARFLAGS = --xz
TAREXT = tar.gz
DISTNAME = lab0-105295708.$(TAREXT)

default: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

lab0.o: lab0.c
	$(CC) $(CFLAGS) -c lab0.c

check: $(TARGET) setup simple_case infile_case outfile_case segfault_case badarg_case bad_infile_case clean
	if [[ -s log.txt ]] ; then \
		echo "One or more test cases failed."; \
	else \
		echo "All test cases passed."; \
	fi; \
	rm -f log.txt

setup:
	rm -f log.txt input.txt output.txt;

simple_case:
	echo "Hello World!" > input.txt; \
	echo "It's a beautiful day." > input.txt; \
	cat input.txt | ./$(TARGET) > output.txt; \
	if ! cmp input.txt output.txt ; then \
		echo "Simple case is wrong." > log.txt; \
	fi; \
	rm -f input.txt output.txt

infile_case:
	echo "Hello World!" > input.txt; \
	echo "It's a beautiful day." > input.txt; \
	./$(TARGET) --input input.txt > output.txt; \
	if ! cmp input.txt output.txt; then \
		echo "File input case is wrong." > log.txt; \
	fi; \
	rm -f input.txt output.txt

outfile_case:
	echo "Hello World!" > input.txt; \
	echo "It's a beautiful day." > input.txt; \
	cat input.txt | ./$(TARGET) --output output.txt; \
	if ! cmp input.txt output.txt; then \
		echo "File output case is wrong." > log.txt; \
	fi; \
	rm -f input.txt output.txt

segfault_case:
	echo "Hello World!" | ./$(TARGET) --segfault --catch 2> /dev/null; \
	if [[ $$? -ne 4 ]]; then \
		echo "Segfault not caught correctly." > log.txt; \
	fi

badarg_case:
	echo "Hello World!" | ./$(TARGET) --invalid 2> /dev/null; \
	if [[ $$? -ne 1 ]]; then \
		echo "Bad argument not handled correctly." > log.txt; \
	fi

bad_infile_case:
	rm -f input.txt; \
	./$(TARGET) --input input.txt 2> /dev/null; \
	if [[ $$? -ne 2 ]]; then \
		echo "Invalid input file not handled correctly." > log.txt; \
	fi

dist: $(DISTNAME)
dist-files = lab0.c Makefile README backtrace.png breakpoint.png
$(DISTNAME): $(dist-files)
	$(TAR) $(TARFLAGS) -cf $@ $(dist-files)

clean:
	rm -f *.o *.$(TAREXT) $(TARGET)