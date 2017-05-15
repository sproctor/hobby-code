#!/usr/bin/env python2.7

import test

import sys

def remove_value(l, v):
    return filter(lambda x: x != v, l)

flatten = lambda l: [item for sublist in l for item in sublist]

def find_siblings(x, y, grid):
    siblings_x = []
    siblings_y = []
    for i in xrange(9):
        if i != x:
            siblings_y.append(grid[i][y])
        if i != y:
            siblings_x.append(grid[x][i])
    siblings_block = []
    for j in xrange(3):
        for k in xrange(3):
            pos_x = x - (x % 3) + j
            pos_y = y - (y % 3) + k
            if pos_x != x or pos_y != y:
                siblings_block.append(grid[pos_x][pos_y])
    return [siblings_x, siblings_y, siblings_block]

def possible_values(x, y, grid):
    if not isinstance(grid[x][y], list) and grid[x][y] > 0:
        return grid[x][y]
    values = range(1, 10)
    for value in flatten(find_siblings(x, y, grid)):
        if not isinstance(value, list):
            values = remove_value(values, value)
    if len(values) == 0:
        print 'Bad grid:'
        display_grid(grid)
        raise ValueError('Element at (' + str(x) + ', ' + str(y) + ') has no possible values.')
    if len(values) == 1:
        return values[0]
    return values

def replace_possibles(grid):
    changed = False
    for x in xrange(9):
        for y in xrange(9):
            if grid[x][y] == 0:
                grid[x][y] = range(1, 10)
            if not isinstance(grid[x][y], list):
                continue
            value = possible_values(x, y, grid)
            if not isinstance(value, list) or len(value) < len(grid[x][y]):
                changed = True
                grid[x][y] = value
    return changed

def replace_if_unique(grid, x, y):
    if not isinstance(grid[x][y], list):
        return False
    for siblings in find_siblings(x, y, grid):
        values = grid[x][y]
        for sibling in siblings:
            for value in values:
                if isinstance(sibling, list):
                    if value in sibling:
                        values = remove_value(values, value)
                else:
                    if value == sibling:
                        values = remove_value(values, sibling)
        if len(values) > 1:
            raise ValueError('Element contains multiple unique values (' + str(x) + ', ' + str(y) + ') values: ' + str(values))
        if len(values) == 1:
            grid[x][y] = values[0]
            return True
    return False

def reduce_grid(grid):
    for x in xrange(9):
        for y in xrange(9):
            if isinstance(grid[x][y], list):
                grid[x][y] = 0
    return grid

def display_grid(grid):
    for x in xrange(9):
        for y in xrange(9):
            sys.stdout.write(str(grid[x][y]) + ' ')
        print

def pairs(values):
    result = []
    for j in xrange(len(values) - 1):
        for k in xrange(j + 1, len(values)):
            result.append([values[j], values[k]])
    return result

def match_doubles(grid, group):
    values = range(1, 10)
    for [x, y] in group:
        if not isinstance(grid[x][y], list):
            values = remove_value(values, grid[x][y])
    for [j, k] in pairs(values):
        elements = []
        for [x, y] in group:
            value = grid[x][y]
            if isinstance(value, list):
                if j in value or k in value:
                    elements.append([x, y])
        if len(elements) == 2:
            print "found double"
            changed = False
            for [x, y] in elements:
                print '[' + str(x) + ', ' + str(y) + '] ' + str(grid[x][y])
                if len(grid[x][y]) > 2:
                    grid[x][y] = [j, k]
                    print grid[x][y]
                    changed = True
            if changed:
                return True
    return False

def match_grid_doubles(grid):
    for x in xrange(9):
        group = []
        for y in xrange(9):
            group.append([x, y])
        if match_doubles(grid, group):
            return True
    for y in xrange(9):
        group = []
        for x in xrange(9):
            group.append([x, y])
        if match_doubles(grid, group):
            return True
    for groupx in xrange(0, 9, 3):
        for groupy in xrange(0, 9, 3):
            group = []
            for x in xrange(3):
                for y in xrange(3):
                    group.append([x, y])
            if match_doubles(grid, group):
                return True
    return False

def remove_number_from_squares(grid, n, squares):
    for [x, y] in squares:
        if isinstance(grid[x][y], list):
            grid[x][y] = remove_value(grid[x][y], n)
    return grid

def squares_outside_block_for_row(block_x, y):
    squares = []
    for x in range(block_x * 3) + range(block_x * 3 + 3, 9):
        squares.append([x, y])
    return squares

def squares_outside_block_for_col(block_y, x):
    squares = []
    for y in range(block_y * 3) + range(block_y * 3 + 3, 9):
        squares.append([x, y])
    return squares

def remove_ineligible_outside_block(grid):
    for block_x in xrange(3):
        for block_y in xrange(3):
            for n in xrange(1, 10):
                #TODO: refactor these two sections
                rows = []
                for x in xrange(block_x * 3, block_x * 3 + 3):
                    for y in xrange(block_y * 3, block_y * 3 + 3):
                        if isinstance(grid[x][y], list) and n in grid[x][y]:
                            rows.append(x)
                            break
                if len(rows) == 1:
                    remove_number_from_squares(grid, n, squares_outside_block_for_row(block_x, rows[0]))
                    return True
                cols = []
                for x in xrange(block_y * 3, block_y * 3 + 3):
                    for y in xrange(block_x * 3, block_x * 3 + 3):
                        if isinstance(grid[x][y], list) and n in grid[x][y]:
                            cols.append(y)
                            break
                if len(cols) == 1:
                    remove_number_from_squares(grid, n, squares_outside_block_for_col(block_y, cols[0]))
                    return True
    return False

def replace_uniques(grid):
    changed = False
    for x in xrange(9):
        for y in xrange(9):
            if replace_if_unique(grid, x, y):
                changed = True
    return changed

def solve(grid):
    changed = True
    while changed:
        changed = False
        if replace_possibles(grid):
            changed = True
            continue
        if replace_uniques(grid):
            changed = True
            continue
        if not changed:
            if match_grid_doubles(grid):
                changed = True
        if not changed:
            if remove_ineligible_outside_block(grid):
                changed = True
    if not is_solved(grid):
        print "Could not solve grid:"
        display_grid(grid)
        raise ValueError('grid unsolved')
    return grid

def is_solved(grid):
    for row in grid:
        for value in row:
            if isinstance(value, list):
                return False
    return True

example_grid = [[0, 0, 3, 0, 2, 0, 6, 0, 0],
                [9, 0, 0, 3, 0, 5, 0, 0, 1],
                [0, 0, 1, 8, 0, 6, 4, 0, 0],
                [0, 0, 8, 1, 0, 2, 9, 0, 0],
                [7, 0, 0, 0, 0, 0, 0, 0, 8],
                [0, 0, 6, 7, 0, 8, 2, 0, 0],
                [0, 0, 2, 6, 0, 9, 5, 0, 0],
                [8, 0, 0, 2, 0, 3, 0, 0, 9],
                [0, 0, 5, 0, 1, 0, 3, 0, 0]]

test.test(possible_values, [0, 0, example_grid], [4, 5])
test.test(possible_values, [8, 8, example_grid], [2, 4, 6, 7])
test.test(possible_values, [8, 5, example_grid], [4, 7])
test.test(possible_values, [8, 7, example_grid], [2, 4, 6, 7, 8])
test.test(pairs, [[1, 2, 3]], [[1, 2], [1, 3], [2, 3]])
test.test(remove_number_from_squares, [[[[1, 2, 3], [2, 3, 4], 9], [4, [5, 2, 4], 3], [[3, 2, 1], 3, 9]], 2, [[0, 0], [0, 1], [2, 0]]],
    [[[1, 3], [3, 4], 9], [4, [5, 2, 4], 3], [[3, 1], 3, 9]])
test.test(squares_outside_block_for_row, [0, 3],
    [[3, 3], [4, 3], [5, 3], [6, 3], [7, 3], [8, 3]])
test.test(squares_outside_block_for_col, [1, 5],
    [[5, 0], [5, 1], [5, 2], [5, 6], [5, 7], [5, 8]])
print "tests completed"

def main():
    with open('p096_sudoku.txt') as f:
        content = f.readlines()

    for n in xrange(50):
        grid = [[int(x) for x in list(content[n * 10 + i + 1].strip())] for i in xrange(9)]
        print grid
        solve(grid)
        print "grid " + str(n + 1) + ':'
        display_grid(grid)

# main()
