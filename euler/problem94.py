#!/usr/bin/python

import sys

def solution(maxp):
    back1 = 17
    back2 = 5
    back3 = 1
    sign = 1
    total = 3 * 5 + 1 + 3 * 17 - 1
    while True:
        p1 = 3 * back1 - sign
        p2 = 3 * back2 + sign
        n = p1 + p2 - back3
        p = 3 * n + sign
        if p > maxp:
            return total
        total += p
        sign *= -1
        back3 = back2
        back2 = back1
        back1 = n

print solution(int(sys.argv[1]))
