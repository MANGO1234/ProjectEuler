import time

from euler_util import *

START = time.time()

# Brute force...
N = 1000000
b = {}
for a in range(1, N):
    m = 1
    c = False
    while True:
        x = (a + m / 3)
        t = 3 * x * x - 4 * m * m / 3
        t = round(t)
        if t <= 0:
            break
        if t >= N:
            break
        if t not in b:
            b[t] = 0
        c = True
        b[t] += 1
        m += 1
    n = 3 * a - 1
    while n > m:
        x = (a + n / 3)
        t = 3 * x * x - 4 * n * n / 3
        t = round(t)
        if t <= 0:
            break
        if t >= N:
            break
        if t not in b:
            b[t] = 0
        c = True
        b[t] += 1
        n -= 1
    if c:
        # print("...", m, n, a)
        continue
    else:
        break

total = 0
for k, v in b.items():
    if v == 10:
        total += 1
print(total)


END = time.time()
print(END - START)
