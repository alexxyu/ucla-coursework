#! /usr/bin/gnuplot
#
# purpose:
#	 generate data reduction graphs for the multi-threaded list project
#
# input: lab2b_list.csv
#	1. test name
#	2. # threads
#	3. # iterations per thread
#	4. # lists
#	5. # operations performed (threads x iterations x (ins + lookup + delete))
#	6. run time (ns)
#	7. run time per operation (ns)
#
# output:
#	lab2b_1.png ... throughput vs. number of threads for mutex and spin-lock synchronized list operations.
#   lab2b_2.png ... mean time per mutex wait and mean time per operation for mutex-synchronized list operations.
#   lab2b_3.png ... successful iterations vs. threads for each synchronization method.
#   lab2b_4.png ... throughput vs. number of threads for mutex synchronized partitioned lists.
#   lab2b_5.png ... throughput vs. number of threads for spin-lock-synchronized partitioned lists.
#
# Note:
#	Managing data is simplified by keeping all of the results in a single
#	file.  But this means that the individual graphing commands have to
#	grep to select only the data they want.
#
#	Early in your implementation, you will not have data for all of the
#	tests, and the later sections may generate errors for missing data.
#

# general plot parameters
set terminal png
set datafile separator ","

# throughput vs number of threads (mutex or spin lock protected)
set title "Lab2b-1: Throughput vs Number of Threads with Synchronization"
set xlabel "Threads"
set logscale x 2
set xrange [0.75:]
set ylabel "Throughput (operations/s)"
set logscale y 10
set output 'lab2b_1.png'

plot \
     "< grep 'list-none-m' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title 'mutex' with linespoints lc rgb 'red', \
     "< grep 'list-none-s' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title 'spin lock' with linespoints lc rgb 'green'

# average wait-for-lock time and operation time against number of threads (w/ mutex)
set title "Lab2b-2: Average Operation Times vs Number of Threads (Mutex)"
set xlabel "Threads"
set logscale x 2
set xrange [0.75:]
set ylabel "Time (ns)"
set logscale y 10
set output 'lab2b_2.png'

plot \
     "< grep 'list-none-m' lab2b_list.csv" using ($2):($8) \
    title 'average wait-for-lock' with linespoints lc rgb 'red', \
     "< grep 'list-none-m' lab2b_list.csv" using ($2):($7) \
    title 'average time/operation' with linespoints lc rgb 'green'

# throughput vs number of threads (mutex protected with sublists)
set title "Lab2b-4: Throughput vs Number of Threads with Mutex and Sublists"
set xlabel "Threads"
set logscale x 2
set xrange [0.75:]
set ylabel "Throughput (operations/s)"
set logscale y 10
set output 'lab2b_4.png'

plot \
     "< grep -e 'list-none-m,[0-9]*,1000,1' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '1 List' with linespoints lc rgb 'red', \
     "< grep -e 'list-none-m,[0-9]*,1000,4' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '4 Sublists' with linespoints lc rgb 'green', \
     "< grep -e 'list-none-m,[0-9]*,1000,8' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '8 Sublists' with linespoints lc rgb 'blue', \
     "< grep -e 'list-none-m,[0-9]*,1000,16' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '16 Sublists' with linespoints lc rgb 'orange'

# throughput vs number of threads (spin lock protected with sublists)
set title "Lab2b-5: Throughput vs Number of Threads with Spin Lock and Sublists"
set xlabel "Threads"
set logscale x 2
set xrange [0.75:]
set ylabel "Throughput (operations/s)"
set logscale y 10
set output 'lab2b_5.png'

plot \
     "< grep -e 'list-none-s,[0-9]*,1000,1' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '1 List' with linespoints lc rgb 'red', \
     "< grep -e 'list-none-s,[0-9]*,1000,4' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '4 Sublists' with linespoints lc rgb 'green', \
     "< grep -e 'list-none-s,[0-9]*,1000,8' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '8 Sublists' with linespoints lc rgb 'blue', \
     "< grep -e 'list-none-s,[0-9]*,1000,16' lab2b_list.csv" using ($2):(1000000000/($7)) \
    title '16 Sublists' with linespoints lc rgb 'orange'