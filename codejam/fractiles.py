#!/usr/bin/python

import sys

def generateFractile(fractile, c):
    if c == 1:
        return fractile
    newFractile = []
    for tile in fractile:
        if tile == 1:
            newFractile += [1 for x in range(len(fractile))]
        else:
            newFractile += fractile
    return generateFractile(newFractile, c - 1)
    
def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    print str(lines)
    print str(generateFractile([1, 0], 1))
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        values = lines.pop(0).split(' ')
        k = int(values[0])
        c = int(values[1])
        s = int(values[2])
        for i in range(k):
            f = generateFractile([1 if x == i else 0 for x in range(k)], c)
            print str(f)

if __name__ == "__main__":
    main()

