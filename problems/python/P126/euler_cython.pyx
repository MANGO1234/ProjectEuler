# format
import time
import numpy as np

from euler_util import *
from libc.string cimport memset

START = time.time()

# the same code in Java is 360ms vs 150ms = 2.4 times slow down which makse sense
cdef int LIMIT = 100000
cdef int a, b, c, i, m, n, cur
cdef int t[100000 + 1]
memset(t, 0, sizeof(t))

for a in range(1, LIMIT + 1):
    for b in range(1, min(LIMIT / a, a) + 1):
        for c in range(1, min((LIMIT - 2 * a * b) / (a + b), b) + 1):
            cur = 2 * (a * b + b * c + a * c)
            i = 0
            while cur <= LIMIT:
                t[cur] += 1
                cur = cur + (a + b + c) * 4 + i
                i += 8

for i, n in enumerate(t):
    if n == 1000:
        print(i)
        break


END = time.time()
print(END - START)
