#!/usr/bin/python

def read_roman(s):
    number = 0
    prev = 1000
    for c in s:
        if c == 'M':
            v = 1000
        elif c == 'D':
            v = 500
        elif c == 'C':
            v = 100
        elif c == 'L':
            v = 50
        elif c == 'X':
            v = 10
        elif c == 'V':
            v = 5
        elif c == 'I':
            v = 1
        number += v
        if v > prev:
            number -= 2 * prev
        prev = v
    return number

def to_roman(n):
    if n >= 1000:
        return "M" + to_roman(n - 1000)
    elif n >= 900:
        return "CM" + to_roman(n - 900)
    elif n >= 500:
        return "D" + to_roman(n - 500)
    elif n >= 400:
        return "CD" + to_roman(n - 400)
    elif n >= 100:
        return "C" + to_roman(n - 100)
    elif n >= 90:
        return "XC" + to_roman(n - 90)
    elif n >= 50:
        return "L" + to_roman(n - 50)
    elif n >= 40:
        return "XL" + to_roman(n - 40)
    elif n >= 10:
        return "X" + to_roman(n - 10)
    elif n == 9:
        return "IX"
    elif n >= 5:
        return "V" + to_roman(n - 5)
    elif n == 4:
        return "IV"
    elif n >= 1:
        return "I" + to_roman(n - 1)
    else:
        assert n == 0
        return ""

def test(function, v, expected):
    r = function(v)
    if not r == expected:
        print "test: " + str(v) + " exptected: " + str(expected) + " got: " + str(r)

test(read_roman, "XXI", 21)
test(read_roman, "XIIIIII", 16)
test(read_roman, "XVI", 16)
test(read_roman, "XIX", 19)
test(read_roman, "XCVI", 96)
test(to_roman, 96, "XCVI")
test(to_roman, 1999, "MCMXCIX")
test(read_roman, "I", 1)
test(read_roman, "V", 5)
test(read_roman, "X", 10)
test(read_roman, "L", 50)
test(read_roman, "C", 100)
test(read_roman, "D", 500)
test(read_roman, "M", 1000)
test(read_roman, "XIX", 19)
test(read_roman, "XLVIIII", 49)
test(read_roman, "XLIX", 49)
test(read_roman, "XXXXVIIII", 49)
test(read_roman, "MCCCCCCVI", 1606)
test(read_roman, "MDCVI", 1606)
test(to_roman, 1606, "MDCVI")
test(to_roman, 49, "XLIX")
test(to_roman, 4444, "MMMMCDXLIV")
test(to_roman, 999, "CMXCIX")

def solution(fname):
    with open(fname) as f:
        lines = f.readlines()
        saved_characters = 0
        for line in lines:
            numeral = line.strip()
            number = read_roman(numeral)
            optimized_numeral = to_roman(number)
            diff = len(numeral) - len(optimized_numeral)
            assert diff >= 0
            saved_characters += diff
        return saved_characters

print solution("p089_roman.txt")
