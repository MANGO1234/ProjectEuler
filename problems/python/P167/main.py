import time

from euler_util import *

START = time.time()

# https://projecteuclid.org/download/pdf_1/euclid.em/1048709116 provides
# everything for the problem
CONSTANT = [None] * 11
CONSTANT[2] = (2 * 2 + 1, 32, 126)
CONSTANT[3] = (2 * 3 + 1, 26, 126)
CONSTANT[4] = (2 * 4 + 1, 444, 1778)
CONSTANT[5] = (2 * 5 + 1, 1628, 6510)
CONSTANT[6] = (2 * 6 + 1, 5906, 23622)
CONSTANT[7] = (2 * 7 + 1, 80, 510)
CONSTANT[8] = (2 * 8 + 1, 126960, 507842)
CONSTANT[9] = (2 * 9 + 1, 380882, 1523526)
CONSTANT[10] = (2 * 10 + 1, 2097152, 8388606)


# (k+1)th odd v
def ulam2Odd(n, k):
    v = n * 2 + 1
    # using bitmask very small speed boost over array
    b = 1
    t = 0
    while t < k:
        n += 1
        tmp = ((b & 1) > 0) ^ ((b & (1 << (v))) > 0)
        if tmp:
            t += 1
        b = ((b << 1) & ((1 << 31) - 1)) + (1 if tmp else 0)
    return n * 2 + 1


# (k+1)th Ulam number for k >> N
def ulam2Large(n, k):
    k -= 2
    return (k // CONSTANT[n][1]) * CONSTANT[n][2] + ulam2Odd(n, k % CONSTANT[n][1])


s = 0
for i in range(2, 11):
    s += ulam2Large(i, pow(10, 11) - 1)
print(s)

END = time.time()
print(END - START)
