#!/usr/bin/python

import array
import math
import sys

def sum_divisors(x):
    total = 1
    for i in xrange(2, int(math.sqrt(x)) + 1):
        if x % i == 0:
            total += i
            n = x / i
            if n != i:
                total += n
    return total

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print "TEST ERROR: " + function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)

test(sum_divisors, [28], 28)
test(sum_divisors, [2], 1)
test(sum_divisors, [4], 3)
test(sum_divisors, [15], 9)
print "Tests passed"

def solution(max_n):
    cache = array.array('l', [sum_divisors(x) for x in xrange(max_n + 1)])
    print "cache created"
    smallest_link = 0
    longest_chain = 0
    for i in xrange(1, max_n + 1):
        length = find_chain(i, max_n, cache)
        if length > longest_chain:
            longest_chain = length
            smallest_link = i
        cache[i] = -1
    return smallest_link

def find_chain(n, max_n, cache):
    length = 0
    link = n
    visited = []
    while link not in visited:
        visited.append(link)
        #prev_link = link
        link = cache[link]
        #cache[prev_link] = -1
        if link < 0 or link > max_n:
            return 0
        length += 1
    if link == n:
        return length
    else:
        return 0

print solution(int(sys.argv[1]))
