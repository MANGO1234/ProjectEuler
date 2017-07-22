import time
import numpy as np

from euler_util import *

START = time.time()

# cython with optimization reached same speed as java port
# but now it takes more time than java to write, so...

cdef int i, j, k, l

t = [[] for i in range(0, 37)]

for i in range(0, 10):
    for j in range(0, 10):
        for k in range(0, 10):
            for l in range(0, 10):
                t[i + j + k + l].append([i,j,k,l])


cdef int f(int n, int b11, int b12, int b13, int b14, int b21, int b22, int b23, int b24):
    cdef int b31, b32, b33, b34, b41, b42, b43, b44, tn=0
    for b31 in range(min(max(n - b11 - b21 - 9, 0), 9), max(min(n - b11 - b21 + 1, 10), 0)):
        b41 = n - b11 - b21 - b31
        b32 = n - b14 - b23 - b41
        if b32 < 0 or b32 > 9:
            continue
        b42 = n - b12 - b22 - b32
        if b42 < 0 or b42 > 9:
            continue
        for b34 in range(min(max(n - b14 - b24 - 9, 0), 9), max(min(n - b14 - b24 + 1, 10), 0)):
            b44 = n - b14 - b24 - b34
            b33 = n - b11 - b22 - b44
            if b33 < 0 or b33 > 9:
                continue
            if b31 + b32 + b33 + b34 != n:
                continue
            b43 = n - b13 - b23 - b33
            if b43 < 0 or b43 > 9:
                continue
            if b41 + b42 + b43 + b44 != n:
                continue
            tn += 1
    return tn


cdef int total = 0
for i in range(0, 18):
    for x in t[i]:
        for y in t[i]:
            total += f(i, x[0],x[1],x[2],x[3],y[0],y[1],y[2],y[3]) * 2
    print(i)
for x in t[18]:
    for y in t[18]:
        total += f(18, x[0],x[1],x[2],x[3],y[0],y[1],y[2],y[3])
print(total)

END = time.time()
print(END - START)
