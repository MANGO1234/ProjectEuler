def subsets(xs):
    a = [[]]
    for x in xs:
        for i in range(0, len(a)):
            s = a[i][:]
            s.append(x)
            a.append(s)
    return a
