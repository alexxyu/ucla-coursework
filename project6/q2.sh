#!/bin/sh
zcat /home/cs143/data/googlebooks-eng-all-1gram-20120701-s.gz | awk '$4 >= 10000' | sort -k 2,2 | head -1 | cut -f 2
