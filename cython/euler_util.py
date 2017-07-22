def subsets(xs):
    a = [[]]
    for x in xs:
        for i in range(0, len(a)):
            s = a[i][:]
            s.append(x)
            a.append(s)
    return a


factorials = [1]


def factorial(n):
    if len(factorials) <= n:
        for i in range(len(factorials), n + 1):
            factorials.append(factorials[i - 1] * i)
    return factorials[n]


def nCr(n, r):
    difference = n - r
    if difference > r:
        difference = r
    product = 1
    for i in range(1, difference + 1):
        product *= n
        product /= i
        n -= 1
    return product
