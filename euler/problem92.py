#!/usr/bin/python

import sys

def is_eightyniner(n, cache):
    if n == 1 or cache[n] == 1:
        return False
    if n == 89 or cache[n] == 2:
        return True
    if is_eightyniner(square_digits(n), cache):
        cache[n] = 2
        return True
    else:
        cache[n] = 1
        return False

def square_digits(n):
    total = 0
    while n > 0:
        digit = n % 10
        n = int(n / 10)
        total += digit ** 2
    return total

def solution(n):
    cache = bytearray(n * 10)
    eightynines = 0
    for i in xrange(2, n + 1):
        if is_eightyniner(i, cache):
            eightynines += 1
    return eightynines

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)

test(square_digits, [85], 89)
test(square_digits, [44], 32)
test(square_digits, [32], 13)
test(square_digits, [13], 10)
test(square_digits, [1], 1)
test(square_digits, [89], 145)
test(square_digits, [145], 42)

test(is_eightyniner, [145, bytearray(1000)], True)
test(is_eightyniner, [44, bytearray(1000)], False)

print solution(int(sys.argv[1]))
