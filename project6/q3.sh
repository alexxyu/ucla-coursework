#!/bin/sh
zcat < googlebooks-eng-all-1gram-20120701-s.gz | head -10000000 | datamash --sort groupby 1 sum 3 | awk '$2 >= 1000000'
