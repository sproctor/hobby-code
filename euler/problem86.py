#!/usr/bin/python

import math
import sys

def distinct_cuboids(a, b, c, m):
    if not is_valid_triple(a, b, c, m):
        return 0
    sum = equivalent_cubes(a, b)
    multiplier = 2
    while is_valid_triple(a * multiplier, b * multiplier, c * multiplier, m):
        sum += equivalent_cubes(a * multiplier, b * multiplier)
        multiplier = multiplier + 1

    # add sums for a = m
    sum += distinct_cuboids(a - 2 * b + 2 * c, 2 * a - b + 2 * c, 2 * a - 2 * b + 3 * c, m)
    sum += distinct_cuboids(a + 2 * b + 2 * c, 2 * a + b + 2 * c, 2 * a + 2 * b + 3 * c, m)
    sum += distinct_cuboids(-a + 2 * b + 2 * c, -2 * a + b + 2 * c, -2 * a + 2 * b + 3 * c, m)
    return sum

def equivalent_cubes(a, b):
    if a > b:
        return equivalent_cubes(b, a)
    r = int(math.floor(2 * a - b) / 2) + 1
    if r < 0:
        return 0
    return r

def is_valid_triple(a, b, c, m):
    if a > m and b > m:
        return False
    if a > 2 * m or b > 2 * m:
        return False
    return True

def shortest_path_is_integer(a, b, c):
    longest = max(a, b, c)
    rest = a + b + c - longest
    return hyp_is_integer(longest, rest)

def hyp_is_integer(x, y):
    return is_square(x ** 2 + y ** 2)

def is_square(x):
    root = math.floor(math.sqrt(x))
    return root * root == x

def solution(p):
    acc = 0
    m = 0
    while acc <= p:
        m = m + 1
        r = distinct_cuboids(m, acc)
        print "m: " + str(m) + " r: " + str(r)
        acc = acc + r
    return m

def test_cubes(a, b, expected):
    r = equivalent_cubes(a, b)
    if not r == expected:
        print "equivalent_cubes(" + str(a) + ", " + str(b) + "); expected: " + str(expected) + " got: " + str(r)

test_cubes(5, 6, 3)

print str(distinct_cuboids(3, 4, 5, 99))
