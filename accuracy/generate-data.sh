#!/bin/sh
echo "name,limit,census,block,valgrind" > $1
# 7
for i in 1 3 4 8 9; do
    ./mk-csv.py $i >> $1
done
