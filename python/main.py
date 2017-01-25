import time

from euler_util import *

START = time.time()

# 15552
count = {}
for n in range(3, 200):
    for a in range(1, n - 1):
        for b in range((n + 1 - a) // 2, min(a + 1, n - a)):
            c = n - a - b
            base = a * b + a * c + b * c
            k = 2 * (n - 1)
            for x in range(0, 10000):
                m = 2 * (base + k * x + 2 * x * x)
                if m > 15552:
                    break
                if m in count:
                    count[m] += 1
                else:
                    count[m] = 1

a = []

for k, v in count.items():
    a.append((k, v))

a.sort()
for (k, v) in a:
    print(k, v)
    if k > 100:
        break
    # if v == 100:
    #     print(k, v)
    #     break


END = time.time()
print(END - START)
