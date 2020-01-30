import math
import random

def random_permutation(n):
    p = list(range(n))
    random.shuffle(p)
    return p

def factor(p):
    npiles = math.ceil(math.sqrt(len(p)))
    q = []
    pilesizes = [0] * npiles
    for i, pi in enumerate(p):
        # the pile index is pi % npiles.
        # But what does that mean about q[i]?
        # I think it means (pi % npiles) * npiles + size of pile?
        q.append((pi % npiles) * npiles + pilesizes[pi % npiles])
        pilesizes[pi % npiles] += 1
    compress(q)

    # and then we _could_ figure out the second pile index, but
    # it seems easier to just use some quick group theory to get
    # the second factor.

    # q * r = p
    # q' * q * r = q' * p
    r = compose(invert(q), p)

    return (q, r)

def compress(p):
    qdict = {pi: i for i, pi in enumerate(p)}
    missing = 0
    for i in range(max(p) + 1):
        if i in qdict:
            p[qdict[i]] -= missing
        else:
            missing += 1

def compose(p, q):
    result = []
    for i in range(len(p)):
        result.append(q[p[i]])
    return result

def invert(p):
    qdict = {pi: i for i, pi in enumerate(p)}
    return [qdict[i] for i in range(len(p))]

def count_piles(p):
    piles = []
    for i in p:
        found_pile = False
        for pile in piles:
            if pile[-1] == i - 1:
                pile.append(i)
                found_pile = True
        if not found_pile:
            piles.append([i])
    return len(piles)

def verify(p):
    q, r = factor(p)
    npiles = math.ceil(math.sqrt(len(p)))
    assert count_piles(q) <= npiles
    assert count_piles(r) <= npiles
    assert compose(q, r) == p
    return True
