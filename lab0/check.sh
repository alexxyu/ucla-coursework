#!/bin/sh

rm -f logfile input.txt output.txt

touch logfile

# simple case:
echo "Hello World!" > input.txt; \
echo "It's a beautiful day." > input.txt; \
cat input.txt | ./lab0 > output.txt; \
if [[ $? -ne 0 ]]; then \
    echo "Wrong exit code for simple case." > logfile; \
fi; \
if ! cmp input.txt output.txt ; then \
    echo "Simple case is wrong." > logfile; \
fi; \
rm -f input.txt output.txt

# infile case:
echo "Hello World!" > input.txt; \
echo "It's a beautiful day." > input.txt; \
./lab0 --input input.txt > output.txt; \
if [[ $? -ne 0 ]]; then \
    echo "Wrong exit code for infile case." > logfile; \
fi; \
if ! cmp input.txt output.txt; then \
    echo "File input case is wrong." > logfile; \
fi; \
rm -f input.txt output.txt

# outfile case:
echo "Hello World!" > input.txt; \
echo "It's a beautiful day." > input.txt; \
cat input.txt | ./lab0 --output output.txt; \
if [[ $? -ne 0 ]]; then \
    echo "Wrong exit code for outfile case." > logfile; \
fi; \
if ! cmp input.txt output.txt; then \
    echo "File output case is wrong." > logfile; \
fi; \
rm -f input.txt output.txt

# segfault case:
echo "Hello World!" | ./lab0 --segfault --catch 2> /dev/null; \
if [[ $? -ne 4 ]]; then \
    echo "Segfault not caught correctly." > logfile; \
fi

# badarg case:
echo "Hello World!" | ./lab0 --invalid 2> /dev/null; \
if [[ $? -ne 1 ]]; then \
    echo "Bad argument not handled correctly." > logfile; \
fi

# bad infile case:
rm -f input.txt; \
./lab0 --input input.txt 2> /dev/null; \
if [[ $? -ne 2 ]]; then \
    echo "Invalid input file not handled correctly." > logfile; \
fi

if [[ -s logfile ]] ; then \
    echo "One or more test cases failed."; \
else \
    echo "All test cases passed."; \
fi; \
rm -f logfile