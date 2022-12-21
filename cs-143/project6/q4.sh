#!/bin/sh
zcat /home/cs143/data/googlebooks-eng-all-1gram-20120701-s.gz | awk '$2 >= 1900' | datamash --full --sort groupby 2 max 3 | cut -f 1,2,3
