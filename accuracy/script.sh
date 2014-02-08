#!/bin/bash
ARG=$1

set -e -x verbose

rm $ARG.txt || true

function run_test {
    echo $T >> $ARG.txt
    echo $N >> $ARG.txt
    echo $K >> $ARG.txt
    valgrind --tool=massif --pages-as-heap=yes --massif-out-file=massif.out ./damp $T $N $K +RTS -T -kc16k >> $ARG.txt
    cat massif.out | grep mem_heap_B | sed -e 's/mem_heap_B=\(.*\)/\1/' | sort -g | tail -n 1 >> $ARG.txt
    #echo 0 >> $ARG.txt
    echo >> $ARG.txt
}

K=1024

case "$ARG" in
    init)
        T=-1; N=0; run_test
        ;;

    runtests)
        for i in 1 3 4 7 8 9; do
            ./script.sh $i
        done
        ;;

    *)
        for K in {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}0000; do
            T=$ARG; N=200000000000; run_test
        done
        ;;
esac
