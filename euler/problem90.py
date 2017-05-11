#!/usr/bin/python

squares = [[0, 1], [0, 4], [0, 9], [1, 6], [2, 5], [3, 6], [4, 9], [6, 4], [8, 1]]


def s():
    combos = 0
    die = [0, 1, 2, 3, 4, 5]
    while die:
        die2 = list(die)
        while die2:
            if is_valid(die, die2):
                combos += 1
                #print str(die) + ", " + str(die2)
            die2 = generate_die(die2)
        die = generate_die(die)
    return combos

def generate_die(original_die):
    die = list(original_die)
    for i in xrange(5, -1, -1):
        if die[i] < 4 + i:
            die[i] += 1
            for j in xrange(i + 1, 6):
                die[j] = die[j - 1] + 1
                if die[j] > 9:
                    return False
            return die
    return False

def is_valid(die1, die2):
    for square in squares:
        a = square[0]
        b = square[1]
        if not ((contains(die1, a) and contains(die2, b)) or (contains(die1, b) and contains(die2, a))):
            return False
    return True

def contains(die, n):
    if n == 6 or n == 9:
        return 6 in die or 9 in die
    return n in die

def test(function, args, expected):
    r = function(*args)
    if r != expected:
        print "Error in test; " + function.__name__ + "(" + str(args) + ") = " + str(r) + "; expected: " + str(expected)

test(generate_die, [[0, 1, 2, 3, 4, 5]], [0, 1, 2, 3, 4, 6])
test(generate_die, [[0, 1, 2, 3, 4, 9]], [0, 1, 2, 3, 5, 6])
test(generate_die, [[0, 1, 2, 3, 5, 9]], [0, 1, 2, 3, 6, 7])
test(generate_die, [[0, 1, 2, 3, 4, 9]], [0, 1, 2, 3, 5, 6])
test(generate_die, [[0, 1, 2, 3, 4, 9]], [0, 1, 2, 3, 5, 6])
test(generate_die, [[0, 1, 2, 3, 4, 9]], [0, 1, 2, 3, 5, 6])
test(generate_die, [[9, 9, 9, 9, 9, 9]], False)
test(is_valid, [[0, 5, 6, 7, 8, 9], [1, 2, 3, 4, 8, 9]], True)
test(is_valid, [[1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 7]], False)

print s()
