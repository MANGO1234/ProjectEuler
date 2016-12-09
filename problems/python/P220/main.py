import time

from euler_util import *

START = time.time()

UP = 0
LEFT = 1
DOWN = 2
RIGHT = 3


def advance(pos, dir):
    (x, y) = pos
    if dir == UP:
        return (x, y + 1)
    elif dir == DOWN:
        return (x, y - 1)
    elif dir == LEFT:
        return (x - 1, y)
    elif dir == RIGHT:
        return (x + 1, y)


def turnLeft(dir):
    if dir == UP:
        return LEFT
    elif dir == DOWN:
        return RIGHT
    elif dir == LEFT:
        return DOWN
    elif dir == RIGHT:
        return UP


def turnRight(dir):
    if dir == UP:
        return RIGHT
    elif dir == DOWN:
        return LEFT
    elif dir == LEFT:
        return UP
    elif dir == RIGHT:
        return DOWN


def transformLeft(dir, pos):
    (x, y) = pos
    if dir == UP:
        return (x, y)
    elif dir == DOWN:
        return (-x, -y)
    elif dir == LEFT:
        return (-y, x)
    elif dir == RIGHT:
        return (y, -x)


def transform(os, dir, pos):
    (osDir, osPos) = os
    osPos = transformLeft(dir, osPos)
    (osX, osY) = osPos
    (x, y) = pos
    dir = (dir + osDir) % 4
    return (dir, (x + osX, y + osY))


cache = {}


def findOffset(ch, count, n):
    if (n, ch) not in cache:
        if ch == "a":
            cache[(n, ch)] = findPos(UP, (0, 0), pow(2, n + 1) - 1, "aRbFR", n)
        if ch == "b":
            cache[(n, ch)] = findPos(UP, (0, 0), pow(2, n + 1) - 1, "LFaLb", n)
    os = cache[(n, ch)]
    totalCount = pow(2, n + 1) - 1
    if totalCount <= count:
        return (os, count - totalCount)
    else:
        if ch == "a":
            return (findPos(UP, (0, 0), count, "aRbFR", n), 0)
        if ch == "b":
            return (findPos(UP, (0, 0), count, "LFaLb", n), 0)


def findPos(dir, pos, count, str, n):
    for ch in str:
        if ch == "F":
            if count == 0:
                return (dir, pos)
            pos = advance(pos, dir)
            count = count - 1
        elif ch == "L":
            dir = turnLeft(dir)
        elif ch == "R":
            dir = turnRight(dir)
        elif n != 0:
            (os, count) = findOffset(ch, count, n - 1)
            (dir, pos) = transform(os, dir, pos)
    if count == 0:
        return (dir, pos)
    else:
        return None


print(findPos(UP, (0, 0), pow(10, 12), "Fa", 50))

END = time.time()
print(END - START)
