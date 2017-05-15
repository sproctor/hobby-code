#!/usr/bin/python

import math
import sys

def distinct_cuboids(a, b, c, m):
    if not is_valid_triple(a, b, m):
        return 0
    sum = equivalent_cubes(a, b, m)
    #print "a: " + str(a) + ", b: " + str(b) + ", c: " + str(c) + ", sum: " + str(sum)
    multiplier = 2
    while is_valid_triple(a * multiplier, b * multiplier, m):
        sum += equivalent_cubes(a * multiplier, b * multiplier, m)
        multiplier = multiplier + 1

    # add sums for a = m
    sum += distinct_cuboids(a - 2 * b + 2 * c, 2 * a - b + 2 * c, 2 * a - 2 * b + 3 * c, m)
    sum += distinct_cuboids(a + 2 * b + 2 * c, 2 * a + b + 2 * c, 2 * a + 2 * b + 3 * c, m)
    sum += distinct_cuboids(-a + 2 * b + 2 * c, -2 * a + b + 2 * c, -2 * a + 2 * b + 3 * c, m)
    return sum

def is_valid_triple(a, b, m):
    if a > m and b > m:
        return False
    if a > 2 * m or b > 2 * m:
        return False
    return True

def equivalent_cubes(a, b, m):
    if a == b:
        return cubes_helper(a, b, m)
    return cubes_helper(a, b, m) + cubes_helper(b, a, m)

def cubes_helper(a, b, m):
    if a > m or b > 2 * a:
        return 0
    if a > b:
        return int(math.floor(b / 2))
    return int(math.floor(2 * a - b) / 2) + 1

def solution(p):
    c = 0
    m = 0
    while c <= p:
        m += 1
        c = distinct_cuboids(3, 4, 5, m)
    return m

def test_cubes(a, b, expected):
    r = equivalent_cubes(a, b, 100)
    if not r == expected:
        print "equivalent_cubes(" + str(a) + ", " + str(b) + "); expected: " + str(expected) + " got: " + str(r)

test_cubes(6, 8, 6)

#print str(distinct_cuboids(3, 4, 5, int(sys.argv[1])))
print str(solution(1000000))
