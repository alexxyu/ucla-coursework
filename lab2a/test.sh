#!/bin/bash

echo "Testing add-none"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y >> lab2_add.csv
    done
done

echo "Testing add-m"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y --sync=m >> lab2_add.csv
    done
done

echo "Testing add-s"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y --sync=s >> lab2_add.csv
    done
done

echo "Testing add-c"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y --sync=c >> lab2_add.csv
    done
done

echo "Testing add-yield"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --yield >> lab2_add.csv
    done
done

echo "Testing add-yield-m"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --yield --sync=m >> lab2_add.csv
    done
done

echo "Testing add-yield-s"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --yield --sync=s >> lab2_add.csv
    done
done

echo "Testing add-yield-c"
for x in 1 2 4 8 16; do 
    for y in 1 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --yield --sync=c >> lab2_add.csv
    done
done

echo "Completed tests and output to lab2_add.csv"
