# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wall -Wextra
OBJS = lab0.o

default: lab0

lab0: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

lab0.o: lab0.c
	$(CC) $(CFLAGS) -c lab0.c

clean:
	rm -f *.o lab0