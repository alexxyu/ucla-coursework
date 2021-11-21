#!/bin/sh
zcat /home/cs143/data/googlebooks-eng-all-1gram-20120701-s.gz | awk '$3 >= 1000*$4' | cut -f 1,2
