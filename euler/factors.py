#!/usr/bin/python

import sys

def factors(n):
    factors = []
    i = 2
    while i * i <= n:
        if n % i == 0:
            n /= i
            factors.append(i)
        else:
            i += 1
    factors.append(n)
    return factors

print factors(int(sys.argv[1]))
