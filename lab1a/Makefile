# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

CC = gcc
CFLAGS = -Wextra -Wall
OBJS = lab1a.o

TARGET = lab1a

default: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

lab1a.o: lab1a.c
	$(CC) $(CFLAGS) -c lab1a.c

clean:
	rm -rf *.o $(OBJS) $(TARGET)
