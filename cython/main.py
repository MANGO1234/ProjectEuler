import pyximport
pyximport.install()

import time
import numpy as np
# python setup.py build_ext --inplace
import euler_cython

from euler_util import *

# START = time.time()

# t = [[] for i in range(0, 37)]

# for i in range(0, 10):
#     for j in range(0, 10):
#         for k in range(0, 10):
#             for l in range(0, 10):
#                 t[i + j + k + l].append([i, j, k, l])


# def f(n, xs):
#     tn = 0
#     for x in xs:
#         for y in xs:
#             for b31 in range(min(max(n - x[0] - y[0] - 9, 0), 9), max(min(n - x[0] - y[0] + 1, 10), 0)):
#                 b41 = n - x[0] - y[0] - b31
#                 b32 = n - x[3] - y[2] - b41
#                 if b32 < 0 or b32 > 9:
#                     continue
#                 b42 = n - x[1] - y[1] - b32
#                 if b42 < 0 or b42 > 9:
#                     continue
#                 for b34 in range(min(max(n - x[3] - y[3] - 9, 0), 9), max(min(n - x[3] - y[3] + 1, 10), 0)):
#                     b44 = n - x[3] - y[3] - b34
#                     b33 = n - x[0] - y[1] - b44
#                     if b33 < 0 or b33 > 9:
#                         continue
#                     if b31 + b32 + b33 + b34 != n:
#                         continue
#                     b43 = n - x[2] - y[2] - b33
#                     if b43 < 0 or b43 > 9:
#                         continue
#                     if b41 + b42 + b43 + b44 != n:
#                         continue
#                     tn += 1
#     return tn


# total = 0
# for i in range(0, 18):
#     total += f(i, t[i]) * 2
#     print(i)
# total += f(18, t[18])
# print(total)

# END = time.time()
# print(END - START)
