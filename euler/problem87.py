#!/usr/bin/python

import numpy as np
import math
import sys

def get_primes(n):
    primes = np.ones(n + 1, dtype=bool)
    for i in xrange(2, int(math.sqrt(n) + 1)):
        if primes[i]:
            for j in xrange(i * i, n + 1, i):
                primes[j] = False
    return primes

def next_prime(n, primes):
    for i in xrange(n + 1, len(primes)):
        if primes[i]:
            return i
    return -1

def prev_prime(n, primes):
    for i in xrange(n - 1, 1, -1):
        if primes[i]:
            return i
    return -1

def s(quad, cube, c, prime_count, primes, maxN):
    #print "s(" + str(square) + ", " + str(cube) + ", " + str(c) + ", " + str(prime_count) + ", ...)"
    while quad + cube + c ** 2 > maxN:
        c = prev_prime(c, primes)
        prime_count -= 1
        if c < 0:
            return [c, 0]
    #print "s(" + str(square) + ", " + str(cube) + ", " + str(c) + ", " + str(prime_count) + ", ...)"
    return [c, prime_count]

def primes_less_than(n, primes):
    count = 0
    for i in xrange(2, int(n) + 1):
        if primes[i]:
            count += 1
    return count

def solution(n):
    sums = np.zeros(n + 1, dtype=bool)
    primes = get_primes(int(math.sqrt(n)))
    a = 2
    quad = a ** 4
    while a > 0 and quad < n:
        b = 2
        cube = b ** 3
        while b > 0 and quad + cube < n:
            c = 2
            square = c ** 2
            while c > 0 and quad + cube + square <= n:
                sums[quad + cube + square] = True
                c = next_prime(c, primes)
                square = c ** 2
            b = next_prime(b, primes)
            cube = b ** 3
        a = next_prime(a, primes)
        quad = a ** 4
    total = 0
    i = 0
    for v in sums:
        if v:
            total += 1
        i += 1
    return total

print solution(int(sys.argv[1]))
