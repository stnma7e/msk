#!/usr/bin/env python

import sys
import re
import matplotlib.pyplot as plt

xs = []
ys = []

with open(sys.argv[1]) as f:
    for i, j in enumerate(f.readlines()):
        for k in re.findall("\d+.\d+", j):
            xs.append(i)
            ys.append(float(k))

plt.scatter(xs, ys)
plt.show()
