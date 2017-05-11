#!/usr/bin/python

import sys

def solution(n):
    triangles = 0
    for x1 in xrange(n + 1):
        for y1 in xrange(n + 1):
            if x1 == 0 and y1 == 0:
                continue
            for x2 in xrange(x1, n + 1):
                yrange = xrange(y1 + 1, n + 1) if x1 == x2 else xrange(n + 1)
                for y2 in yrange:
                    if is_right_triangle(x1, y1, x2, y2):
                        triangles += 1
    return triangles

def is_right_triangle(x1, y1, x2, y2):
    side1sq = x1 ** 2 + y1 ** 2
    side2sq = x2 ** 2 + y2 ** 2
    side3sq = (x2 - x1) ** 2 + (y2 - y1) ** 2
    return side1sq + side2sq == side3sq or side1sq + side3sq == side2sq or side2sq + side3sq == side1sq

print solution(int(sys.argv[1]))
