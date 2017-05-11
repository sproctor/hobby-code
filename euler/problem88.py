#!/usr/bin/python

# this solution is slow but works

from operator import mul
import time
import sys
import numpy as np

def minimal_product_sum(k):
    n = k + 2
    while not is_product_sum(n, k):
        n += 1
    return n

def is_product_sum(n, k):
    return test_factors(factorize(n), n, k)

def test_factors(l, n, k):
    # print "test_factors(" + str(l) + ", " + str(n) + ", " + str(k) + ") r = " + str( sum(l) + k - len(l))
    sum_factors = sum(l)
    num_factors = len(l)
    ones = k - num_factors
    if sum_factors + ones == n:
        return True
    if num_factors < 2 or sum_factors + ones > n:
        return False
    head = l[0]
    for i in xrange(1, len(l)):
        if test_factors([head * l[i]] + l[1:i] + l[i + 1:], n, k):
            return True
    return test_factors(l[1:], n - head, k - 1)

def factorize(n):
    i = 2
    factors = []
    while i * i <= n:
        if n % i == 0:
            factors.insert(0, i)
            n = int(n / i)
        else:
            i += 1
    factors.insert(0, n)
    return factors

def test(function, n, expected):
    r = function(n)
    if not r == expected:
        print "test: " + str(n) + " exptected: " + str(expected) + " got: " + str(r)

def do_tests():
    test(factorize, 4, [2, 2])
    test(factorize, 5, [5])
    test(factorize, 6, [3, 2])
    test(factorize, 8, [2, 2, 2])
    test(factorize, 10, [5, 2])

    test(minimal_product_sum, 2, 4)
    test(minimal_product_sum, 3, 6)
    test(minimal_product_sum, 4, 8)
    test(minimal_product_sum, 5, 8)
    test(minimal_product_sum, 6, 12)
    test(minimal_product_sum, 7, 12)

def solution(n):
    sums = np.zeros(n * 2 + 1, dtype=bool)
    for i in xrange(2, n + 1):
        ps = minimal_product_sum(i)
        sums[ps] = True
        #print "sum: " + str(ps) + " k: " + str(i)
    answer = 0
    for i in xrange(2, len(sums)):
        if sums[i]:
            answer += i
    return answer

do_tests()
#print minimal_product_sum(int(sys.argv[1]))
print solution(int(sys.argv[1]))
