#!/bin/bash
# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

LISTFN=lab2b_list.csv

rm $LISTFN 2> /dev/null

#Mutex synchronized list operations, 1,000 iterations, 1,2,4,8,12,16,24 threads
#Spin-lock synchronized list operations, 1,000 iterations, 1,2,4,8,12,16,24 threads

echo "Testing mutex and sync protected operations"
for x in 1 2 4 8 12 16 24; do
    ./lab2_list --threads=$x --iterations=1000 --sync=m >> $LISTFN
done
for x in 1 2 4 8 12 16 24; do
    ./lab2_list --threads=$x --iterations=1000 --sync=s >> $LISTFN
done

echo "Completed tests and output to $LISTFN"
