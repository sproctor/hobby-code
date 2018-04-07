#!/usr/bin/python

import sys
from array import array

def solve(stack):
    flips = 0
    for pancake in reversed(stack):
        if flips % 2 == 0 and pancake == '-' or flips % 2 == 1 and pancake == '+':
            flips += 1
    return flips

def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        line = lines.pop(0)
        print "Case #" + str(i + 1) + ": " + str(solve(line))

if __name__ == "__main__":
    main()
