#!/usr/bin/python
import os
import sys

from riplib import *

rs = []
r = []
for l in open(sys.argv[1] + ".txt"):
    l = l.strip()
    if l == "":
        rs.append(r)
        r = []
    else:
        r.append(int(l))

for r in rs:
    print("%s,%d,%d,%d,%d" % (lookup[r[ix_t]], r[ix_k]*4096, delta(r,ix_census), delta(r,ix_blocks), delta(r,ix_valgrind)))
