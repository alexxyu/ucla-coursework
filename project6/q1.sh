#!/bin/sh
zcat < googlebooks-eng-all-1gram-20120701-s.gz | head -10000000 | awk '$3 >= 1000*$4' | cut -f 1,2
