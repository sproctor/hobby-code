#!/usr/bin/python

import sys

maxHeight = 2500

def solve(bffs, start):
    if len(bffs) == 0:
        return 0
    chainLen = [0 for x in bffs]
    currLen = 1
    pos = bffs[start]
    while chainLen[pos - 1] == 0:
        print str(pos)
        chainLen[pos - 1] = currLen
        currLen += 1
        if pos == bffs[pos - 1]:
            return currLen
        pos = bffs[pos - 1]
    return currLen - chainLen[pos - 1]

def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        studentCount = int(lines.pop(0))
        bffs = []
        for bff in lines.pop(0).split(' '):
            bffs.append(int(bff))
        maxCircle = 0
        for j in range(studentCount):
            s = solve(bffs, j)
            if s > maxCircle:
                maxCircle = s
        print "Case #" + str(i + 1) + ": " + str(maxCircle)

if __name__ == "__main__":
    main()

