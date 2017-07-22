import time
import numpy as np

from euler_util import *

START = time.time()

# basic idea: calculate each probability of ant being at a square after 50 rounds, then for each square
# calculate expected value of no ant being there and sum up all the squares
# no taking into account symmetry but it's super fast to code

# (899/900)^900*900 will offer an answer to the nearest integer (330)
# probably over the long term, each ant will have 1/900 of being at a square

x = []

for i in range(0, 30):
    a = []
    for j in range(0, 30):
        k = np.zeros((30, 30))
        k[i][j] = 1
        a.append(k)
    x.append(a)

t = [x]

for i in range(1, 51):
    a = []
    for j in range(0, 30):
        b = []
        for k in range(0, 30):
            b.append(None)
        a.append(b)
    t.append(a)


def f(n, i, j):
    if t[n][i][j] is not None:
        return t[n][i][j]
    if i == 0:
        if j == 0:
            t[n][i][j] = f(n - 1, i + 1, j) / 2 + f(n - 1, i, j + 1) / 2
        elif j == 29:
            t[n][i][j] = f(n - 1, i + 1, j) / 2 + f(n - 1, i, j - 1) / 2
        else:
            t[n][i][j] = f(n - 1, i + 1, j) / 3 + f(n - 1, i, j - 1) / 3 + f(n - 1, i, j + 1) / 3
    elif i == 29:
        if j == 0:
            t[n][i][j] = f(n - 1, i - 1, j) / 2 + f(n - 1, i, j + 1) / 2
        elif j == 29:
            t[n][i][j] = f(n - 1, i - 1, j) / 2 + f(n - 1, i, j - 1) / 2
        else:
            t[n][i][j] = f(n - 1, i - 1, j) / 3 + f(n - 1, i, j - 1) / 3 + f(n - 1, i, j + 1) / 3
    elif j == 0:
        t[n][i][j] = f(n - 1, i, j + 1) / 3 + f(n - 1, i - 1, j) / 3 + f(n - 1, i + 1, j) / 3
    elif j == 29:
        t[n][i][j] = f(n - 1, i, j - 1) / 3 + f(n - 1, i - 1, j) / 3 + f(n - 1, i + 1, j) / 3
    else:
        t[n][i][j] = f(n - 1, i, j - 1) / 4 + f(n - 1, i, j + 1) / 4 + f(n - 1, i - 1, j) / 4 + f(n - 1, i + 1, j) / 4
    return t[n][i][j]

for i in range(0, 30):
    for j in range(0, 30):
        f(50, i, j)

t = t[50]
n = 0
for i1 in range(0, 30):
    for j1 in range(0, 30):
        tmp = 1
        for i2 in range(0, 30):
            for j2 in range(0, 30):
                tmp *= (1 - t[i2][j2][i1][j1])
        n += tmp
print(n)

END = time.time()
print(END - START)
