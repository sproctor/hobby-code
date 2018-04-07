#!/usr/bin/python

import sys
from array import array

def checkArray(digitsSeen):
    for x in digitsSeen:
        if x == 0:
            return False
    return True

def solve(n):
    digitsSeen = array('b', [0 for x in range(10)])
    for i in range(1, 100):
        currentNumber = i * n
        digits = getDigits(currentNumber)
        for x in digits:
            if not digitsSeen[x]:
                digitsSeen[x] = 1
                if checkArray(digitsSeen):
                    return str(currentNumber)
    return "INSOMNIA"

def getDigits(x):
    digits = []
    while x > 0:
        digits.append(x % 10)
        x /= 10
    return digits

def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        x = int(lines.pop(0))
        print "Case #" + str(i + 1) + ": " + solve(x)

if __name__ == "__main__":
    main()
