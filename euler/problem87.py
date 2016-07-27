#!/usr/bin/python

import numpy as np
import math
import sys

def get_primes(n):
    primes = np.ones(n + 1, dtype=bool)
    for i in xrange(2, int(math.sqrt(n))):
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

def s(square, cube, c, prime_count, primes, maxN):
    #print "s(" + str(square) + ", " + str(cube) + ", " + str(c) + ", " + str(prime_count) + ", ...)"
    while square + cube + c ** 4 > maxN:
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
    a = 2
    square = a ** 2
    b = 2
    cube = b ** 3
    primes = get_primes(n)
    c = prev_prime(n + 1, primes)
    print "c: " + str(c)
    prime_count = primes_less_than(c + 1, primes)
    sums = 0
    while a > 0 and c > 0 and square + cube + 2 ** 4 <= n:
        saved_c = c
        saved_count = prime_count
        while b > 0 and c > 0 and square + cube + 2 ** 4 <= n:
            [c, prime_count] = s(square, cube, c, prime_count, primes, n)
            sums += prime_count
            b = next_prime(b, primes)
            cube = b ** 3
        a = next_prime(a, primes)
        square = a ** 2
        b = 2
        cube = b ** 3
        c = saved_c
        prime_count = saved_count
    return sums

print solution(int(sys.argv[1]))
