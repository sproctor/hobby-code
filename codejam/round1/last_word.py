#!/usr/bin/python

import sys

def solve(word):
    lastWord = word[:1]
    firstLetter = word[0]
    for c in word[1:]:
        if c >= firstLetter:
            lastWord = str(c) + lastWord
            firstLetter = c
        else:
            lastWord += str(c) 
    return lastWord

def main():
    filename = sys.argv[1]
    filehandle = open(filename, 'r')
    lines = filehandle.readlines()
    numberOfTests = int(lines.pop(0))
    for i in range(numberOfTests):
        line = lines.pop(0)
        lastWord = solve(line.strip())
        print "Case #" + str(i + 1) + ": " + lastWord

if __name__ == "__main__":
    main()

