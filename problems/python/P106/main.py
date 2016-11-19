import time

from euler_util import *

START = time.time()


# count forms of [1,3,5] [2,4,6] = no need to do equality check
def f(n, m):
    return f_h(0, n, m, 0, 0)


def f_h(i, n, m, p, q):
    # print(i, n, m, p, q)
    if p == m and q == m:
        return 1
    if i == n or p > m or q > m:
        return 0
    if i + m + m - p - q > n:
        return 0
    k = f_h(i + 1, n, m, p, q)
    k += f_h(i + 1, n, m, p + 1, q)
    if q + 1 <= p:
        k += f_h(i + 1, n, m, p, q + 1)
    return k

n = 12
k = 0
for i in range(2, n // 2 + 1):
    k += nCr(n, i) * nCr(n - i, i) / 2 - f(n, i)
print(k)


END = time.time()
print(END - START)
