#!/bin/bash
# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

LISTFN=lab2b_list.csv

rm -rf $LISTFN

echo "Testing mutex and sync protected operations"
for x in 1 2 4 8 12 16 24; do
    ./lab2_list --threads=$x --iterations=1000 --sync=m >> $LISTFN
done
for x in 1 2 4 8 12 16 24; do
    ./lab2_list --threads=$x --iterations=1000 --sync=s >> $LISTFN
done

echo "Testing list partitioning (id yield, no synchronization)"
for x in 1 4 8 12 16; do
    for y in 1 2 4 8 16; do
        ./lab2_list --threads=$x --iterations=$y --yield=id --lists=4 >> $LISTFN 2> /dev/null
    done
done

echo "Testing list partitioning (id yield, with synchronization)"
for x in 1 4 8 12 16; do
    for y in 10 20 40 80; do
        ./lab2_list --threads=$x --iterations=$y --yield=id --lists=4 --sync=m >> $LISTFN 2> /dev/null
        ./lab2_list --threads=$x --iterations=$y --yield=id --lists=4 --sync=s >> $LISTFN 2> /dev/null
    done
done

echo "Testing list partitioning (no yield, with synchronization)"
for x in 1 2 4 8 12; do
    for y in 4 8 16; do
        ./lab2_list --threads=$x --iterations=1000 --lists=$y --sync=m >> $LISTFN 2> /dev/null
        ./lab2_list --threads=$x --iterations=1000 --lists=$y --sync=s >> $LISTFN 2> /dev/null
    done
done

echo "Completed tests and output to $LISTFN"
