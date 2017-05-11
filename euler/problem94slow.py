#!/usr/bin/python

import math
import sys

def is_integral_area(a, diff):
    #area = math.sqrt((3 * a + diff) * (a - diff)) * (a + diff) / 4
    return is_perfect_square((3 * a + diff) * (a - diff) / 4)

def is_perfect_square(n):
    sq = int(math.sqrt(n))
    return sq ** 2 == n

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print "TEST ERROR! " + function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)

test(is_integral_area, [5, 1], True)
test(is_integral_area, [6, 1], False)
test(is_integral_area, [17, -1], True)
test(is_integral_area, [17, 1], False)
test(is_integral_area, [241, -1], True)
test(is_integral_area, [901, 1], True)

def solution(n):
    a = 3
    total = 0
    while 3 * a - 1 <= n:
        if is_integral_area(a, -1):
            total += 3 * a - 1
            print "valid for " + str(a) + ", -1 = " + str((3 * a - 1) * (a + 1)) + " perimeter: " + str(3 * a - 1)
            #print "valid for " + str(a) + ", -1"
        if 3 * a + 1 <= n:
            if is_integral_area(a, 1):
                total += 3 * a + 1
                print "valid for " + str(a) + ", 1 = " + str((3 * a + 1) * (a - 1)) + " perimeter: " + str(3 * a + 1)
        a += 2
    return total

print solution(int(sys.argv[1]))
