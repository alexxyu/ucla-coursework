#!/bin/sh
zcat < googlebooks-eng-all-1gram-20120701-s.gz | head -10000000 | awk '$2 >= 1900' | datamash --full --sort groupby 2 max 3 | cut -f 1,2,3
