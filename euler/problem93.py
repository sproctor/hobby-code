#!/usr/bin/python

from __future__ import division
from operator import mul, add, sub
import numpy as np

def s(digits):
    cache = np.zeros(10000, dtype=bool)
    for a in digits:
        for b in digits:
            if b == a:
                continue
            for c in digits:
                if c == b or c == a:
                    continue
                for d in digits:
                    if d == a or d == b or d == c:
                        continue
                    find_values([a, b, c, d], cache)
    for i in xrange(1, len(cache)):
        if not cache[i]:
            return i - 1

def find_values(digits, cache):
    operators = [add, sub, div, mul]
    for op1 in operators:
        for op2 in operators:
            for op3 in operators:
                #print_values(digits, [op1, op2, op3])
                all_orders(digits, [op1, op2, op3], cache)

def print_values(digits, ops):
    if len(ops) > 0:
        print digits[0], " ", ops[0].__name__, " ",
        print_values(digits[1:], ops[1:])
    else:
        print digits[0]

def all_orders(numbers, operators, cache):
    if len(operators) == 0:
        result = numbers[0]
        if result > 0 and (isinstance(result, (int, long)) or result.is_integer()):
            cache[int(result)] = True
    for i in xrange(len(operators)):
        try:
            n = operators[i](numbers[i], numbers[i + 1])
            new_numbers = numbers[:i] + [n] + numbers[i + 2:]
            new_operators = operators[:i] + operators[i + 1:]
            all_orders(new_numbers, new_operators, cache)
        except ZeroDivisionError:
            None

def solution(x):
    best = 0
    for a in xrange(x + 1):
        for b in xrange(a + 1, x + 1):
            for c in xrange(b + 1, x + 1):
                for d in xrange(c + 1, x + 1):
                    n = s([a, b, c, d])
                    if n > best:
                        best = n
                        result = str(a) + str(b) + str(c) + str(d)
    print best
    return result

def div(a, b):
    return a / b

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print "TEST ERROR: " + function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)

test(s, [[1, 2, 3, 4]], 28)

print solution(9)
