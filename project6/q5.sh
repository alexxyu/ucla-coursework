#!/bin/sh
zcat < googlebooks-eng-all-1gram-20120701-s.gz | head -10000000 | grep -v '_' | awk '$2 >= 2000' | datamash --sort groupby 1 sum 3 | sort -k 2,2rn | head -10
