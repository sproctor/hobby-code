#!/usr/bin/python

import sys
from array import array

def getFactor(n):
    d = 2
    i = 0
    wheel = [1,2,2,4,2,4,2,4,6,2,6]
    while d*d <= n:
        if n % d == 0:
            return d
        d += wheel[i]
        i = 3 if i == 10 else i + 1
    return 0

def getValue(base, i):
    if i == 0:
        return 0
    return (i % 2) + base * getValue(base, i / 2)

def getJamFactors(i):
    factors = []
    for base in range(2, 11):
        value = getValue(base, i)
        factor = getFactor(value)
        if factor == 0:
            return None
        #print value
        factors.append(str(factor))
    return factors

def solve(n, count):
    jams = 0
    for i in range(2 ** (n - 2)):
        factors = getJamFactors((2 ** (n - 1)) + 2 * i + 1)
        if factors != None:
            jams += 1
            jam = '1'
            for j in range(n-2):
                jam = ('0' if i % 2 == 0 else '1') + jam
                i /= 2
            jam = '1' + jam
            print jam + " " + " ".join(factors)
            if jams >= count:
                return

def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        line = lines.pop(0)
        numbers = line.split(' ')
        n = int(numbers[0])
        j = int(numbers[1])
        print "Case #" + str(i + 1) + ":"
        solve(n, j)

if __name__ == "__main__":
    main()
