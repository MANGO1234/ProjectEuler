import time
import fractions

from euler_util import *

START = time.time()

# more brute force
b = {}
for i in range(2, 900):
    for j in range(1, i):
        if (i - j) % 2 == 1 and fractions.gcd(i, j) == 1:
            t = 2 * i * i + 2 * i * j
            for x in range(t, 1500001, t):
                if x not in b:
                    b[x] = 0
                b[x] += 1
s = 0
for k, v in b.items():
    if v == 1:
        s += 1
print(s)


END = time.time()
print(END - START)
