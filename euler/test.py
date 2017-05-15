#!/usr/bin/env python2.7

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print "TEST ERROR: " + function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)
