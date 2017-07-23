import pyximport
pyximport.install()

import time
import array
# python setup.py build_ext --inplace
import euler_cython

from euler_util import *

# exact same code is 50s vs 0.15s in cython = 330 times difference, how slow can python be for numerical work

# START = time.time()

# LIMIT = 100000
# t = [0] * (LIMIT + 1)

# for a in range(1, LIMIT + 1):
#     for b in range(1, min(LIMIT // a, a) + 1):
#         for c in range(1, min((LIMIT - 2 * a * b) // (a + b), b) + 1):
#             cur = 2 * (a * b + b * c + a * c)
#             m = 4 * (a + b + c)
#             i = 0
#             while cur <= LIMIT:
#                 t[cur] += 1
#                 cur = cur + m + i
#                 i += 8

# for i, n in enumerate(t):
#     if n == 1000:
#         print(i)
#         break

# END = time.time()
# print(END - START)
