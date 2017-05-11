#!/usr/bin/python

import array
import sys

def sum_divisors(x):
    total = 0
    for i in xrange(1, x / 2 + 1):
        if x % i == 0:
            total += i
    return total

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print "TEST ERROR: " + function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)

test(sum_divisors, [28], 28)
test(sum_divisors, [2], 1)
test(sum_divisors, [4], 3)
test(sum_divisors, [15], 9)

def solution(n):
    cache = array.array('l', [0 for x in xrange(n + 1)])
    for i in xrange(1, n + 1):
        v = sum_divisors(i)
        if v > n:
            cache[i] = -1
        else:
            cache[i] = v
    smallest_link = 0
    longest_chain = 0
    for i in xrange(1, n + 1):
        [value, length] = find_chain(i, cache)
        if length > longest_chain:
            longest_chain = length
            smallest_link = value
    return smallest_link

def find_chain(n, cache):
    length = 1
    smallest_link = n
    link = cache[n]
    while link != n:
        if link < 0:
            return [0, 0]
        if link < smallest_link:
            smallest_link = link
        link = cache[link]
    fill 
    return [smallest_link, length]

print solution(int(sys.argv[1]))
