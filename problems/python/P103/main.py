import time

from euler_util import *

START = time.time()


def is_special_sum(s):
    ps = subsets(s)
    ps = list(map(lambda x: (sum(x), len(x)), ps))
    ps.sort(key=lambda x: x[0])
    for i in range(0, len(ps) - 1):
        if ps[i][0] == ps[i + 1][0] or ps[i][1] > ps[i + 1][1]:
            return False
    return True


# had a bug, should be fixed now
def is_special_sum2(s):
    s.sort()
    for i in range(2, len(s)):
        if sum(s[:i]) <= sum(s[(-i + 1):]):
            return False
    ps = subsets(s)
    ps = list(map(lambda x: (len(x), sum(x)), ps))
    ps.sort()
    for i in range(0, len(ps) - 1):
        if ps[i][1] == ps[i + 1][1]:
            return False
    return True


def f():
    for i in range(10, 40):
        for j in range(i + 1, 40):
            for k in range(j + 1, 40):
                for l in range(k + 1, 40):
                    for m in range(l + 3, i + j):
                        for n in range(l + 2, min(i + j + k - m, m)):
                            for o in range(l + 1, min(i + j + k + l - m - n, n)):
                                if is_special_sum([i, j, k, l, m, n, o]):
                                    print([i, j, k, l, o, n, m])
                                    print(
                                        "".join(map(lambda x: str(x), [i, j, k, l, o, n, m])))
                                    return

f()

END = time.time()
print(END - START)
