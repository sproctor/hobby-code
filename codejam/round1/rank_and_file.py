#!/usr/bin/python

import sys

maxHeight = 2500

def solve(heights):
    missingList = []
    for i in range(maxHeight + 1):
        if heights[i] % 2 == 1:
            missingList.append(str(i))
    return missingList

def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        n = int(lines.pop(0))
        heights = dict([(x, 0) for x in range(maxHeight + 1)])
        for j in range(n * 2 - 1):
            line = lines.pop(0)
            heightStrings = line.split(' ')
            heightList = []
            for k in range(n):
                heights[int(heightStrings[k])] += 1
        missingList = solve(heights)
        print "Case #" + str(i + 1) + ": " + " ".join(missingList)

if __name__ == "__main__":
    main()

