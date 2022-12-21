#!/bin/bash
# NAME: Alex Yu
# EMAIL: alexy23@g.ucla.edu
# ID: 105295708

ADDFN=lab2_add.csv
LISTFN=lab2_list.csv

rm $ADDFN $LISTFN 2> /dev/null

echo "Testing add-none"
for x in 1 2 4 8 12; do 
    for y in 10 20 40 80 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y >> $ADDFN 2> /dev/null
    done
done

echo "Testing add-yield-none"
for x in 1 2 4 8 12; do 
    for y in 10 20 40 80 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y --yield >> $ADDFN 2> /dev/null
    done
done

echo "Testing add-yield-m"
for x in 1 2 4 8 12; do 
    for y in 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --yield --sync=m >> $ADDFN 2> /dev/null
    done
done

echo "Testing add-yield-s"
for x in 1 2 4 8 12; do 
    for y in 10 100 1000; do 
        ./lab2_add --threads=$x --iterations=$y --yield --sync=s >> $ADDFN 2> /dev/null
    done
done

echo "Testing add-yield-c"
for x in 1 2 4 8 12; do 
    for y in 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --yield --sync=c >> $ADDFN 2> /dev/null
    done
done


echo "Testing add-m"
for x in 1 2 4 8 12; do 
    for y in 10 100 1000 10000 100000; do 
        ./lab2_add --threads=$x --iterations=$y --sync=m >> $ADDFN 2> /dev/null
    done
done

echo "Testing add-s"
for x in 1 2 4 8 12; do 
    for y in 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --sync=s >> $ADDFN 2> /dev/null
    done
done

echo "Testing add-c"
for x in 1 2 4 8 12; do 
    for y in 10 100 1000 10000; do 
        ./lab2_add --threads=$x --iterations=$y --sync=c >> $ADDFN 2> /dev/null
    done
done

echo "Completed add tests and output to $ADDFN"

yield_options=(i l d il dl id ild)
echo "Testing list-none-none"
for y in 1 10 100 1000 10000 20000; do 
    ./lab2_list --threads=1 --iterations=$y >> $LISTFN 2> /dev/null
done
for x in 2 4 8 12; do 
    for y in 1 10 100 1000; do 
        ./lab2_list --threads=$x --iterations=$y >> $LISTFN 2> /dev/null
    done
done

echo "Testing list-none-m"
for x in 1 2 4 8 12 16 24; do 
    for y in 1 10 100 1000; do 
        ./lab2_list --threads=$x --iterations=$y --sync=m >> $LISTFN 2> /dev/null
    done
done

echo "Testing list-none-s"
for x in 1 2 4 8 12 16 24; do 
    for y in 1 10 100 1000; do 
        ./lab2_list --threads=$x --iterations=$y --sync=s >> $LISTFN 2> /dev/null
    done
done

echo "Testing list-yield-none"
for z in ${yield_options[@]}; do
    for x in 1 2 4 8 12; do 
        for y in 1 2 4 8 16 32; do 
            ./lab2_list --threads=$x --iterations=$y --yield=$z >> $LISTFN 2> /dev/null
        done
    done
done

echo "Testing list-yield-m"
for z in ${yield_options[@]}; do
    for x in 2 4 8 12; do 
        for y in 1 2 4 8 16 32; do 
            ./lab2_list --threads=$x --iterations=$y --yield=$z --sync=m >> $LISTFN 2> /dev/null
        done
    done
done

echo "Testing list-yield-s"
for z in ${yield_options[@]}; do
    for x in 2 4 8 12; do 
        for y in 1 2 4 8 16 32; do 
            ./lab2_list --threads=$x --iterations=$y --yield=$z --sync=s >> $LISTFN 2> /dev/null
        done
    done
done

echo "Completed list tests and output to $LISTFN"