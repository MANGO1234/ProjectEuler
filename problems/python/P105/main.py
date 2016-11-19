import time

from euler_util import *

START = time.time()

with open("p105_sets.txt") as f:
    content = f.readlines()
    content = list(
        map(lambda x: list(map(lambda x: int(x), x[:-1].split(","))), content))


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


k = 0
for s in content:
    if is_special_sum2(s):
        k += sum(s)
print(k)

END = time.time()
print(END - START)
