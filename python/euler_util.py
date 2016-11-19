def subsets(xs):
    a = [[]]
    for x in xs:
        b = list(map(lambda aa: aa[:], a))
        for s in b:
            s.append(x)
            a.append(s)
    return a
