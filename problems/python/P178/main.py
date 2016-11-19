import time
import numpy

START = time.time()

S = 40
n = numpy.zeros((S, 10, 1024))
for i in range(0, 10):
    n[0][i][1 << i] = 1
for i in range(0, S - 1):
    for j in range(0, 10):
        for k in range(0, 1024):
            t = n[i][j][k]
            if t != 0:
                if j < 9:
                    n[i + 1][j + 1][k | (1 << (j + 1))] += t
                if j > 0:
                    n[i + 1][j - 1][k | (1 << (j - 1))] += t

print(n[:, 1:, 1023].sum())

END = time.time()
print(END - START)
